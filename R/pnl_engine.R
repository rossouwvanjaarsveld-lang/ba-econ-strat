# =============================================================================
#  pnl_engine.R -- quarterly balance-sheet, earnings, capital and ROE engine
#
#  One engine serves the sector calibration, NGFB's scenario projections, the
#  macro sensitivities and the optimiser (via finite differences), so every
#  number in the report comes from the same arithmetic.
#
#  Balance sheet lines carry their own pricing rule:
#     asset   : yield = reference rate + spread   (reference: prime, float3m
#               -- the 3-month floating benchmark, JIBAR to its last fixing on
#               31 December 2026 and compounded ZARONIA + 16.19 bp after --,
#               repo, tbill, gov10y, swap5y, or "none" for non-earning lines)
#     deposit : cost  = alpha + beta * repo       (sector deposit betas)
#     funding : cost  = reference rate + spread
#  Credit losses = sector credit-loss ratio x line multiplier x average loans.
#  A structural hedge (receive-fixed, rolling boxcar, paying the floating
#  benchmark) sits on top of NII.
#  The balance sheet is kept balanced by a funding "plug" line.
# =============================================================================

#' Template of the columns a balance-sheet definition must carry
#' @return Character vector of required column names.
#' @export
bs_columns <- function() {
  c("line", "side", "balance", "ref", "spread", "alpha", "beta", "rw",
    "clr_mult", "runoff", "hqla", "asf", "rsf", "earning")
}

#' Reference-rate lookup for one quarter
#' @param rates One-row data frame (or list) of rates in percent.
#' @param ref Reference names.
#' @return Numeric vector of decimal rates (`NA` -> 0 for "none").
#' @export
ref_rate <- function(rates, ref) {
  r <- vapply(ref, function(k) {
    if (is.na(k) || k %in% c("none", "deposit", "")) return(0)
    v <- if (k == "float3m") float_rate(rates) else rates[[k]]
    if (is.null(v) || !is.finite(v)) NA_real_ else as.numeric(v)
  }, numeric(1))
  r / 100
}

#' Run the quarterly P&L engine
#'
#' @param bs Balance-sheet definition at the jump-off (see [bs_columns()]);
#'   balances in rand (any consistent unit).
#' @param growth Data frame: `date` plus one column per line holding the
#'   quarterly growth factor of that line's balance (1.02 = +2\% q/q). Lines
#'   absent from `growth` grow with total assets; the `plug` line absorbs the
#'   funding gap.
#' @param rates Data frame: `date`, and reference rates in percent (repo,
#'   float3m -- or, in older inputs, jibar --, prime, tbill, gov10y, swap5y).
#' @param credit Data frame: `date`, `clr` (sector credit-loss ratio, percent
#'   of average loans, annualised).
#' @param ratios Data frame: `date`, `nir_assets`, `opex_assets` (percent of
#'   average assets, annualised).
#' @param plug Name of the line absorbing the funding gap.
#' @param hedge List with `notional` (rand), `tenor_q` (quarters) and
#'   `fixed0` (average fixed rate on the existing book, percent) -- or `NULL`.
#'   Optional tranche form: `new_ref` (the `rates` column at which maturing
#'   tranches are reinvested, e.g. "swap7y"), `new_tenor_q`, an
#'   `extra_notional` struck now in `extra_tenor_q` quarterly tranches at a
#'   fixed `extra_rate` (e.g. the market rate at the jump-off) or else at the
#'   first quarter's `extra_ref` rate (negative notional: pay-fixed swaps that
#'   offset part of the book; the net maturing notional is what gets
#'   reinvested),
#'   and a `target_notional` below the current book, reached by not
#'   replacing maturing tranches. For strategies that change the hedge
#'   over time: `target_path`, the net notional to hold after each quarter
#'   (one value per quarter; each quarter's new tranche closes the gap --
#'   receive-fixed to add, pay-fixed to reduce -- at that quarter's rate,
#'   and overrides `target_notional`), and `new_ref` and `new_tenor_q` may be
#'   vectors with one value per quarter.
#' @param tax Corporate tax rate.
#' @param payout Dividend payout ratio on positive earnings: a number, one
#'   number per quarter, or a function `f(t, state)` of the quarter and the
#'   state at the end of the previous quarter (`state$cet1`, the CET1 ratio;
#'   `state$equity`; `state$date`), for payout rules.
#' @param cet1_share CET1 as a share of book equity.
#' @param op_risk_alpha Basic-indicator operational-risk factor (0 = off).
#' @param rwa_other Non-credit RWA (market, operational, other) as a share of
#'   total assets, calibrated to BA700 less BA200.
#' @param line_rates Optional data frame `date` + one column per line with
#'   that line's rate (percent p.a.) in each quarter; overrides the pricing
#'   rule for those lines (used to feed Stage 1 sector yields and costs).
#' @param overlay Optional data frame for positions held outside the line
#'   book (the bond books of treasury.R), one row per quarter: `date`, `nii`
#'   (net interest income in rand added in the quarter: the books' carry over
#'   the Treasury bills they replace) and optionally `gains` (realised gains,
#'   negative for losses, on bonds sold in the quarter, pre-tax: recycled
#'   from OCI to profit or loss, as IFRS 9 requires for debt held at fair
#'   value through OCI) and `hqla_mtm` (the books' cumulative mark-to-market,
#'   realised and unrealised, pre-tax: the amount by which the HQLA stock at
#'   market value differs from its book value, for the LCR), `exposure`
#'   (rand held on balance sheet outside the line book, for the leverage
#'   ratio) and `rsf` (required stable funding those holdings add).
#' @param buffer Optional list for sizing the liquidity buffer: `release`,
#'   the share of the HQLA line's balance released in each quarter (one value
#'   per quarter, a level, not an increment), `line` (default "hqla") and
#'   `scale`, the line's earning share (the rest is cash and central-bank
#'   balances). The released balance is earning Treasury bills: it leaves
#'   the line, the funding plug falls by the same amount (the bills repay
#'   wholesale funding), and the line's rate is reduced so that the income
#'   lost is the bills' full yield. Fee income, operating costs and
#'   non-credit RWA, which the engine scales with total assets, are computed
#'   on the balance sheet before the release: Treasury bills held or sold do
#'   not change the cost base or the operational and market risk.
#' @return List: `quarterly` (one row per quarter with NII, NIM, credit losses,
#'   NIR, opex, PBT, NI, equity, RWA, CET1 ratio, ROE, LCR, NSFR, the leverage
#'   exposure and the leverage ratio with CET1 as the measure of capital, ...),
#'   `balances` (line x quarter), `lines` (line-level income/expense).
#' @export
run_engine <- function(bs, growth, rates, credit, ratios, plug = "wholesale_other",
                       hedge = NULL, tax = 0.27, payout = 0.5, cet1_share = 0.88,
                       op_risk_alpha = 0, rwa_other = 0, line_rates = NULL, overlay = NULL,
                       buffer = NULL) {
  miss <- setdiff(bs_columns(), names(bs))
  if (length(miss)) stop("balance sheet is missing columns: ", paste(miss, collapse = ", "))
  dates <- rates$date
  nq <- length(dates)
  L <- nrow(bs)
  B <- matrix(NA_real_, L, nq + 1L, dimnames = list(bs$line, NULL))
  B[, 1] <- bs$balance
  is_a <- bs$side == "asset"; is_l <- bs$side %in% c("deposit", "funding", "liability")
  is_e <- bs$side == "equity"
  pl <- match(plug, bs$line)
  if (is.na(pl)) stop("plug line not found: ", plug)
  loans <- is_a & bs$clr_mult > 0

  out <- vector("list", nq)
  inc_mat <- matrix(0, L, nq, dimnames = list(bs$line, as.character(dates)))
  E <- sum(bs$balance[is_e])
  hedge_book <- if (!is.null(hedge)) rep(hedge$fixed0, hedge$tenor_q) else NULL
  ## tranche form of the hedge (when a reinvestment curve point is given):
  ## the jump-off book is `tenor_q` equal tranches at `fixed0` maturing one a
  ## quarter; each maturing tranche is reinvested for `new_tenor_q` quarters
  ## at the swap rate in column `new_ref` of `rates` (e.g. "swap7y"). With
  ## new_ref = "swap5y" and new_tenor_q = tenor_q = 20 it reproduces the
  ## boxcar exactly. An optional `extra_notional` (e.g. a hedge of equity) is
  ## struck at the first quarter's `extra_ref` rate in `extra_tenor_q`
  ## quarterly tranches.
  tranche <- !is.null(hedge) && !is.null(hedge$new_ref)
  if (tranche) {
    tr_not <- rep(hedge$notional / hedge$tenor_q, hedge$tenor_q)
    tr_rate <- rep(hedge$fixed0, hedge$tenor_q); tr_mat <- seq_len(hedge$tenor_q)
    if (!is.null(hedge$extra_notional) && hedge$extra_notional != 0) {  # e.g. an equity hedge built now; negative = a pay-fixed overlay
      k <- hedge$extra_tenor_q %||% hedge$tenor_q
      tr_not <- c(tr_not, rep(hedge$extra_notional / k, k)); tr_mat <- c(tr_mat, seq_len(k))
      tr_rate <- c(tr_rate, rep(NA_real_, k))                           # struck at the first quarter's rates
    }
  }
  gi_hist <- numeric(0)
  rel_amt <- 0                                   # rand released from the buffer, end of the previous quarter
  if (!is.null(buffer)) {
    hb <- match(buffer$line %||% "hqla", bs$line)
    if (is.na(hb)) stop("run_engine: buffer line not found")
    rel <- rep_len(buffer$release, nq); sc_b <- buffer$scale %||% 1
    if (any(rel < 0 | rel >= sc_b)) stop("run_engine: a buffer release must lie in [0, the line's earning share)")
  }
  ## CET1 ratio at the jump-off (the state a payout rule sees in quarter 1)
  rwa_0 <- sum(bs$balance * bs$rw * is_a) + rwa_other * sum(bs$balance[is_a])
  cet1_prev <- cet1_share * E / rwa_0

  for (t in seq_len(nq)) {
    prev <- B[, t]
    ta_prev <- sum(prev[is_a])
    g <- rep(NA_real_, L)
    gr <- growth[growth$date == dates[t], , drop = FALSE]
    for (i in seq_len(L)) if (bs$line[i] %in% names(gr)) g[i] <- gr[[bs$line[i]]]
    # lines without an explicit path grow with modelled assets (excluding plug)
    a_known <- is_a & is.finite(g)
    g_ta <- if (any(a_known)) sum(prev[a_known] * g[a_known]) / sum(prev[a_known]) else 1
    g[!is.finite(g)] <- g_ta
    cur <- prev * g
    rel_prev <- rel_amt; rel_amt <- 0
    if (!is.null(buffer)) {                    # release (or rebuild) Treasury bills held in the buffer
      r_was <- if (t == 1L) 0 else rel[t - 1L]
      pre_h <- cur[hb] / (1 - r_was)           # the line's balance had nothing been released
      rel_amt <- pre_h * rel[t]
      cur[hb] <- pre_h * (1 - rel[t])
    }
    cur[is_e] <- prev[is_e]                                  # equity rolls below
    # funding plug keeps assets = liabilities + equity
    gap <- sum(cur[is_a]) - sum(cur[is_l]) - sum(prev[is_e])
    if (bs$side[pl] %in% c("deposit", "funding", "liability")) cur[pl] <- cur[pl] + gap
    else cur[pl] <- cur[pl] - gap
    if (cur[pl] < 0) {                                       # plug exhausted: move to HQLA
      h <- which(bs$hqla > 0 & is_a)[1]
      if (!is.na(h)) cur[h] <- cur[h] - cur[pl]
      cur[pl] <- 0
    }
    avg <- (prev + cur) / 2
    rt <- rates[t, , drop = FALSE]
    base <- ref_rate(rt, bs$ref)
    y <- base + bs$spread
    dep <- bs$side == "deposit"
    y[dep] <- bs$alpha[dep] + bs$beta[dep] * as.numeric(rt$repo) / 100
    if (!is.null(line_rates)) {                 # scenario-specific line rates (percent)
      lr <- line_rates[line_rates$date == dates[t], , drop = FALSE]
      if (nrow(lr)) for (i in which(bs$line %in% names(lr))) {
        v <- as.numeric(lr[[bs$line[i]]])
        if (is.finite(v)) y[i] <- v / 100
      }
    }
    if (!is.null(buffer)) {                    # the released balance earned the full bill rate
      r_avg <- (rel[t] + (if (t == 1L) 0 else rel[t - 1L])) / 2
      y[hb] <- y[hb] * (sc_b - r_avg) / (sc_b * (1 - r_avg))
    }
    y[!bs$earning] <- 0
    flow <- avg * y / 4
    ii <- sum(flow[is_a]); ie <- sum(flow[is_l])
    # structural hedge: receive the book's fixed rates, pay the floating
    # benchmark (JIBAR, then compounded ZARONIA + CAS: converted and fallback
    # swaps pay ZARONIA plus the 16.19 bp spread on the floating leg)
    hedge_pl <- 0; h_not <- if (!is.null(hedge)) hedge$notional else 0; h_pv01 <- NA_real_
    if (tranche) {
      jb <- as.numeric(float_rate(rt))
      nref <- if (length(hedge$new_ref) > 1L) hedge$new_ref[t] else hedge$new_ref
      ntq <- hedge$new_tenor_q %||% hedge$tenor_q
      if (length(ntq) > 1L) ntq <- ntq[t]
      new_rate <- as.numeric(rt[[nref]])
      if (!length(new_rate) || !is.finite(new_rate)) stop("run_engine: rates have no column ", nref)
      tr_rate[!is.finite(tr_rate)] <- hedge$extra_rate %||% as.numeric(rt[[hedge$extra_ref %||% nref]])
      tr_mat <- tr_mat - 1L
      roll <- tr_mat <= 0L
      matured <- sum(tr_not[roll])
      tr_not <- tr_not[!roll]; tr_rate <- tr_rate[!roll]; tr_mat <- tr_mat[!roll]
      reinv <- if (!is.null(hedge$target_path)) {
        hedge$target_path[t] - sum(tr_not)               # close the gap to the path, either way
      } else if (is.null(hedge$target_notional)) matured else
        ## a lower target notional is reached by not replacing maturing tranches
        max(0, min(matured, hedge$target_notional - sum(tr_not)))
      if (abs(reinv) > 1e-9 * max(1, abs(hedge$notional))) {
        tr_not <- c(tr_not, reinv); tr_rate <- c(tr_rate, new_rate); tr_mat <- c(tr_mat, ntq)
      }
      hedge_pl <- sum(tr_not * (tr_rate - jb)) / 100 / 4
      h_not <- sum(tr_not)
      ## the book's PV01 (rand per basis point of the swap curve) at the end of
      ## the quarter: quarterly annuities over each tranche's remaining life,
      ## discounted at a flat 7.5%
      h_pv01 <- 1e-4 * sum(tr_not * (1 - (1 + 0.075 / 4)^(-pmax(tr_mat, 0))) / 0.075)
    } else if (!is.null(hedge) && hedge$notional > 0) {
      new_fixed <- as.numeric(rt$swap5y %||% float_rate(rt))
      hedge_book <- c(hedge_book[-1], new_fixed)
      hedge_pl <- hedge$notional * (mean(hedge_book) - as.numeric(float_rate(rt))) / 100 / 4
    }
    ovr <- if (is.null(overlay)) NULL else overlay[overlay$date == dates[t], , drop = FALSE]
    ov <- if (is.null(ovr)) 0 else sum(as.numeric(ovr$nii))
    ov_g <- if (is.null(ovr$gains)) 0 else sum(as.numeric(ovr$gains))
    ov_m <- if (is.null(ovr$hqla_mtm)) 0 else sum(as.numeric(ovr$hqla_mtm))
    ov_x <- if (is.null(ovr$exposure)) 0 else sum(as.numeric(ovr$exposure))
    ov_r <- if (is.null(ovr$rsf)) 0 else sum(as.numeric(ovr$rsf))
    nii <- ii - ie + hedge_pl + ov
    clr_t <- as.numeric(credit$clr[credit$date == dates[t]]) / 100
    imp <- sum(avg[loans] * bs$clr_mult[loans]) * clr_t / 4
    ta_avg <- sum(avg[is_a])
    ta_ops <- ta_avg + (rel_prev + rel_amt) / 2          # the cost base ignores released bills
    rr <- ratios[ratios$date == dates[t], , drop = FALSE]
    nir <- ta_ops * as.numeric(rr$nir_assets) / 100 / 4
    opex <- ta_ops * as.numeric(rr$opex_assets) / 100 / 4
    pbt <- nii + nir + ov_g - opex - imp
    ni <- pbt * (1 - tax)
    po <- if (is.function(payout)) payout(t, list(cet1 = cet1_prev, equity = E, date = dates[t])) else
      if (length(payout) > 1L) payout[t] else payout
    div <- if (ni > 0) po * ni else 0
    E_new <- E + ni - div
    cur[is_e] <- E_new * bs$balance[is_e] / sum(bs$balance[is_e])
    # retained earnings displace plug funding (or are invested in the plug asset)
    dE <- E_new - E
    if (bs$side[pl] %in% c("deposit", "funding", "liability")) cur[pl] <- cur[pl] - dE
    else cur[pl] <- cur[pl] + dE
    gi_hist <- c(gi_hist, nii + nir)
    gi_ann <- 4 * mean(utils::tail(gi_hist, 4))
    rwa_cr <- sum(cur * bs$rw * is_a)
    rwa <- rwa_cr + 12.5 * op_risk_alpha * gi_ann + rwa_other * (sum(cur[is_a]) + rel_amt)
    iea <- sum(avg[is_a & bs$earning])
    liq <- liquidity_ratios(cur, bs, hqla_adj = ov_m, rsf_adj = ov_r)   # LCR and NSFR proxies, HQLA at market (liquidity.R)
    expo <- sum(cur[is_a]) + ov_x                    # leverage exposure: on-balance-sheet assets
    inc_mat[, t] <- ifelse(is_a, flow, -flow)
    out[[t]] <- data.frame(
      date = dates[t], total_assets = sum(cur[is_a]), iea = iea,
      interest_income = ii, interest_expense = ie, hedge_pl = hedge_pl, hedge_notional = h_not,
      hedge_pv01 = h_pv01, overlay_nii = ov, overlay_gains = ov_g, nii = nii,
      nim = 4 * nii / iea, asset_yield = 4 * ii / iea,
      cost_of_funds = 4 * ie / sum(avg[is_l]),
      impairments = imp, clr = 4 * imp / sum(avg[loans]),
      nir = nir, opex = opex, cost_to_income = opex / (nii + nir),
      pbt = pbt, net_income = ni, dividends = div, equity = E_new,
      roe = 4 * ni / ((E + E_new) / 2), roa = 4 * ni / ta_avg,
      rwa = rwa, cet1_ratio = cet1_share * E_new / rwa,
      lcr = liq[["lcr"]], nsfr = liq[["nsfr"]],
      exposure = expo, leverage = cet1_share * E_new / expo,
      loans = sum(cur[loans]), deposits = sum(cur[dep]), plug_balance = cur[pl],
      stringsAsFactors = FALSE)
    E <- E_new; cet1_prev <- cet1_share * E_new / rwa
    B[, t + 1L] <- cur
  }
  q <- do.call(rbind, out)
  colnames(B) <- c("t0", as.character(dates))
  list(quarterly = q, balances = B, lines = inc_mat)
}

#' Summarise an engine run over a horizon
#' @param run Output of [run_engine()].
#' @param quarters Which quarters (indices) to summarise; default all.
#' @return One-row data frame of horizon averages (ROE, NIM, CLR, CTI) and
#'   end-point capital and liquidity.
#' @export
summarise_run <- function(run, quarters = NULL) {
  q <- run$quarterly
  if (!is.null(quarters)) q <- q[quarters, , drop = FALSE]
  data.frame(roe = mean(q$roe), nim = mean(q$nim), clr = mean(q$clr),
             cost_to_income = sum(q$opex) / sum(q$nii + q$nir),
             net_income = sum(q$net_income), cet1_end = utils::tail(q$cet1_ratio, 1),
             cet1_min = min(q$cet1_ratio), lcr_min = min(q$lcr),
             roe_y1 = mean(utils::head(q$roe, 4)), roe_end = utils::tail(q$roe, 1))
}
