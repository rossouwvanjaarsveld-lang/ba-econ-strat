# =============================================================================
#  pnl_engine.R -- quarterly balance-sheet, earnings, capital and ROE engine
#
#  One engine serves the sector calibration, NGFB's scenario projections, the
#  macro sensitivities and the optimiser (via finite differences), so every
#  number in the report comes from the same arithmetic.
#
#  Balance sheet lines carry their own pricing rule:
#     asset   : yield = reference rate + spread   (reference: prime, jibar,
#               repo, tbill, gov10y, swap5y, or "none" for non-earning lines)
#     deposit : cost  = alpha + beta * repo       (sector deposit betas)
#     funding : cost  = reference rate + spread
#  Credit losses = sector credit-loss ratio x line multiplier x average loans.
#  A structural hedge (receive-fixed, rolling boxcar) sits on top of NII.
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
    v <- rates[[k]]
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
#'   jibar, prime, tbill, gov10y, swap5y).
#' @param credit Data frame: `date`, `clr` (sector credit-loss ratio, percent
#'   of average loans, annualised).
#' @param ratios Data frame: `date`, `nir_assets`, `opex_assets` (percent of
#'   average assets, annualised).
#' @param plug Name of the line absorbing the funding gap.
#' @param hedge List with `notional` (rand), `tenor_q` (quarters) and
#'   `fixed0` (average fixed rate on the existing book, percent) -- or `NULL`.
#' @param tax Corporate tax rate.
#' @param payout Dividend payout ratio on positive earnings.
#' @param cet1_share CET1 as a share of book equity.
#' @param op_risk_alpha Basic-indicator operational-risk factor (0 = off).
#' @param rwa_other Non-credit RWA (market, operational, other) as a share of
#'   total assets, calibrated to BA700 less BA200.
#' @param line_rates Optional data frame `date` + one column per line with
#'   that line's rate (percent p.a.) in each quarter; overrides the pricing
#'   rule for those lines (used to feed Stage 1 sector yields and costs).
#' @return List: `quarterly` (one row per quarter with NII, NIM, credit losses,
#'   NIR, opex, PBT, NI, equity, RWA, CET1 ratio, ROE, LCR, ...),
#'   `balances` (line x quarter), `lines` (line-level income/expense).
#' @export
run_engine <- function(bs, growth, rates, credit, ratios, plug = "wholesale_other",
                       hedge = NULL, tax = 0.27, payout = 0.5, cet1_share = 0.88,
                       op_risk_alpha = 0, rwa_other = 0, line_rates = NULL) {
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
  gi_hist <- numeric(0)

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
    y[!bs$earning] <- 0
    flow <- avg * y / 4
    ii <- sum(flow[is_a]); ie <- sum(flow[is_l])
    # structural hedge: receive the boxcar's average fixed rate, pay jibar
    hedge_pl <- 0
    if (!is.null(hedge) && hedge$notional > 0) {
      new_fixed <- as.numeric(rt$swap5y %||% rt$jibar)
      hedge_book <- c(hedge_book[-1], new_fixed)
      hedge_pl <- hedge$notional * (mean(hedge_book) - as.numeric(rt$jibar)) / 100 / 4
    }
    nii <- ii - ie + hedge_pl
    clr_t <- as.numeric(credit$clr[credit$date == dates[t]]) / 100
    imp <- sum(avg[loans] * bs$clr_mult[loans]) * clr_t / 4
    ta_avg <- sum(avg[is_a])
    rr <- ratios[ratios$date == dates[t], , drop = FALSE]
    nir <- ta_avg * as.numeric(rr$nir_assets) / 100 / 4
    opex <- ta_avg * as.numeric(rr$opex_assets) / 100 / 4
    pbt <- nii + nir - opex - imp
    ni <- pbt * (1 - tax)
    E_new <- E + ni - if (ni > 0) payout * ni else 0
    cur[is_e] <- E_new * bs$balance[is_e] / sum(bs$balance[is_e])
    # retained earnings displace plug funding (or are invested in the plug asset)
    dE <- E_new - E
    if (bs$side[pl] %in% c("deposit", "funding", "liability")) cur[pl] <- cur[pl] - dE
    else cur[pl] <- cur[pl] + dE
    gi_hist <- c(gi_hist, nii + nir)
    gi_ann <- 4 * mean(utils::tail(gi_hist, 4))
    rwa_cr <- sum(cur * bs$rw * is_a)
    rwa <- rwa_cr + 12.5 * op_risk_alpha * gi_ann + rwa_other * sum(cur[is_a])
    iea <- sum(avg[is_a & bs$earning])
    liq <- liquidity_ratios(cur, bs)                 # LCR and NSFR proxies (liquidity.R)
    inc_mat[, t] <- ifelse(is_a, flow, -flow)
    out[[t]] <- data.frame(
      date = dates[t], total_assets = sum(cur[is_a]), iea = iea,
      interest_income = ii, interest_expense = ie, hedge_pl = hedge_pl, nii = nii,
      nim = 4 * nii / iea, asset_yield = 4 * ii / iea,
      cost_of_funds = 4 * ie / sum(avg[is_l]),
      impairments = imp, clr = 4 * imp / sum(avg[loans]),
      nir = nir, opex = opex, cost_to_income = opex / (nii + nir),
      pbt = pbt, net_income = ni, equity = E_new,
      roe = 4 * ni / ((E + E_new) / 2), roa = 4 * ni / ta_avg,
      rwa = rwa, cet1_ratio = cet1_share * E_new / rwa,
      lcr = liq[["lcr"]], nsfr = liq[["nsfr"]],
      loans = sum(cur[loans]), deposits = sum(cur[dep]),
      stringsAsFactors = FALSE)
    E <- E_new
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
