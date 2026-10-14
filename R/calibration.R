# =============================================================================
#  calibration.R -- tie the engine's free parameters to the sector returns
#
#  The engine prices most lines with Stage 1 sector yields and costs that are
#  BA120-consistent by construction. The remaining free choices are pinned to
#  the sector at the calibration quarter (the last BA120 quarter):
#
#    * credit risk weights are scaled so that the sector's BA900 balance sheet
#      reproduces BA200 credit RWA; non-credit RWA = BA700 total - BA200 credit
#    * relative credit-loss multipliers are normalised to average one over the
#      sector loan mix, then scaled to the BA100/BA900 loan-base ratio
#    * CET1 / book equity from BA700 and BA100
#    * the yield on HQLA (share of non-earning reserves) and the wholesale
#      funding spread are solved so that engine interest income and expense
#      match BA120 over the four quarters to the calibration date
#    * the tax rate is the sector's effective rate over the last three years
#    * the LCR and NSFR proxies are scaled so that the sector's proxies equal
#      the ratios the Prudential Authority publishes (liquidity.R)
# =============================================================================

#' Sector balance sheet (engine lines) at a date from the group panel
#' @param gp Group panel with a "Sector" entity.
#' @param date Quarter-end date.
#' @return Data frame `line, side, balance`.
#' @export
sector_balance_sheet <- function(gp, date) {
  s <- gp[gp$entity == "Sector" & gp$date == as.Date(date), ]
  assets <- ngfb_asset_lines()
  dep <- c("beh_operational", "beh_notice", "beh_savings", "beh_term")
  a <- vapply(assets, function(v) pmax(s[[v]], 0), 0)
  a["corporate_loans"] <- a["corporate_loans"] + s$impairments          # impairments are negative
  d <- vapply(dep, function(v) s[[v]], 0)
  other_dep <- s$dep_retail + s$dep_corporate + s$dep_financial + s$dep_government +
    s$dep_interbank + s$dep_foreign - sum(d)
  fund <- c(repo_funding = s$repo_funding, wholesale_other = s$wholesale_other + other_dep,
            other_liabilities = s$other_liabilities)
  eq <- s$equity
  out <- rbind(data.frame(line = names(a), side = "asset", balance = as.numeric(a)),
               data.frame(line = names(d), side = "deposit", balance = as.numeric(d)),
               data.frame(line = names(fund), side = "funding", balance = as.numeric(fund)),
               data.frame(line = "equity", side = "equity", balance = eq))
  gap <- sum(out$balance[out$side == "asset"]) - sum(out$balance[out$side != "asset"])
  out$balance[out$line == "other_liabilities"] <- out$balance[out$line == "other_liabilities"] + gap
  out
}

#' Calibrate engine parameters to the sector
#'
#' @param P Panels list from analysis/01 (needs `gp`, `spnl`, `capital`,
#'   `ba200_last`, `s1pan`, and `depmat` for the liquidity calibration).
#' @param date Calibration quarter (default: last BA120 quarter).
#' @param chars Line characteristics.
#' @param lcr_target,nsfr_target Published sector LCR and NSFR at `date`
#'   (decimals); `NULL` skips the liquidity calibration.
#' @param flagged Optional named list: series -> dates flagged as one-off
#'   outliers by the Stage 1 impulse dummies (replaced by the window median).
#' @return List: `chars` (calibrated credit parameters; deposit liquidity
#'   factors are entity-specific, see [entity_chars()]), `rwa_other`,
#'   `cet1_share`, `tax`, `hqla_scale`, `wholesale_spread`, `liq`, `checks`.
#' @export
calibrate_to_sector <- function(P, date = NULL, chars = line_characteristics(),
                                lcr_target = NULL, nsfr_target = NULL, flagged = list()) {
  sp <- P$spnl
  if (is.null(date)) date <- max(sp$date[is.finite(sp$nii)])
  date <- as.Date(date)
  bal <- sector_balance_sheet(P$gp, date)
  B <- stats::setNames(bal$balance, bal$line)
  ta <- sum(bal$balance[bal$side == "asset"])
  cap <- P$capital[P$capital$date == date, ]
  b2 <- P$ba200_last
  credit_rwa <- b2$value[grepl("^total$", tolower(trimws(b2$item_desc)))][2]
  ## 1. risk weights
  rw <- stats::setNames(chars$rw, chars$line)
  raw_cr <- sum(rw[names(B)] * B, na.rm = TRUE)
  k_rw <- credit_rwa / raw_cr
  chars$rw <- chars$rw * k_rw
  rwa_other <- (cap$rwa - credit_rwa) / ta
  ## 2. credit-loss multipliers: average one on the sector mix, then scaled to
  ##    the BA100 loan base used in the sector CLR
  m <- stats::setNames(chars$clr_mult, chars$line)
  ln <- names(m)[m > 0]
  avg_m <- sum(m[ln] * B[ln]) / sum(B[ln])
  loans_ba100 <- sp$gross_loans[sp$date == date]
  scale_base <- loans_ba100 / sum(B[ln])
  chars$clr_mult <- chars$clr_mult / avg_m * scale_base
  ## 3. CET1 share of book equity
  cet1_share <- cap$cet1_ratio / 100 * cap$rwa / sp$equity_ordinary[sp$date == date]
  ## 4. tax
  w <- sp[sp$date > date - 3 * 365.25 & sp$date <= date, ]
  tax <- sum(w$tax) / sum(w$pbt)
  ## 5. HQLA: the share held as (non-earning) cash and central-bank balances
  ##    comes from BA100; the earning part yields the T-bill rate.
  ##    Wholesale funding spread: the one free level parameter, solved so that
  ##    the engine's sector NII over the four quarters to the calibration date
  ##    equals BA120 NII. Loans and deposits keep their BA120 product yields
  ##    and costs. Gross interest income and expense both come out above
  ##    BA120's because BA900 carries trading-book positions on both sides of
  ##    the balance sheet whose interest BA120 books in trading revenue (its
  ##    own "allocated to trading" lines); the excess is close to net-neutral,
  ##    so it is reported rather than calibrated away. Quarters a satellite
  ##    flags as one-off outliers (impulse dummies) are replaced by the window
  ##    median of that series first.
  q4 <- P$s1pan[P$s1pan$date > date - 365 & P$s1pan$date <= date, ]
  q4$tbill <- q4$jibar3m - 0.15
  q4_raw <- q4
  for (v in intersect(names(flagged), names(q4))) {
    bad <- q4$date %in% as.Date(flagged[[v]])
    if (any(bad) && any(!bad)) q4[[v]][bad] <- stats::median(q4[[v]][!bad])
  }
  cash_frac <- sp$cash_cb[sp$date == date] / B[["hqla"]]
  hq <- max(0, min(1, 1 - cash_frac))
  pr <- line_pricing_map(); pr$hqla$scale <- hq
  a_lines <- setdiff(bal$line[bal$side == "asset"], "other_assets")
  l_lines <- bal$line[bal$side %in% c("deposit", "funding") & bal$line != "other_liabilities"]
  nii_target <- sum(sp$nii[sp$date %in% q4$date])
  inc_target <- sum(sp$ii_total[sp$date %in% q4$date])
  exp_target <- sum(sp$ie_total[sp$date %in% q4$date])
  flows <- function(rules, qq = q4) {
    inc <- 0; ex <- 0
    for (i in seq_len(nrow(qq))) {
      inc <- inc + sum(vapply(a_lines, function(l) B[[l]] * line_rate(rules[[l]], qq[i, ]) / 400, 0))
      ex <- ex + sum(vapply(l_lines, function(l) B[[l]] * line_rate(rules[[l]], qq[i, ]) / 400, 0))
    }
    c(inc = inc, ex = ex, nii = inc - ex)
  }
  f0 <- flows(pr)
  # NII is linear in the wholesale spread: solve in closed form
  solve_ws <- function(f) pr$wholesale_other$spread_fixed +
    (f[["nii"]] - nii_target) / (nrow(q4) * B[["wholesale_other"]] / 400)
  ws <- solve_ws(f0)
  ws_raw <- solve_ws(flows(pr, q4_raw))          # without neutralising flagged outliers
  pr$wholesale_other$spread_fixed <- ws
  fl <- flows(pr)
  ## 6. liquidity: depositor-weighted Basel factors, scaled to the published ratios
  liq <- list(k_lcr = 1, k_nsfr = 1)
  if (!is.null(lcr_target) && !is.null(nsfr_target) && !is.null(P$depmat)) {
    lf <- deposit_liquidity_factors(P$depmat[P$depmat$entity == "Sector", ], date)
    ch_s <- liquidity_chars(chars, lf)
    liq <- calibrate_liquidity(bal, ch_s, lcr_target, nsfr_target)
    liq$targets <- c(lcr = lcr_target, nsfr = nsfr_target)
    liq$sector_factors <- lf
  }
  checks <- data.frame(
    item = c("credit RWA scale (BA200 / raw weights)", "non-credit RWA, % of assets",
             "CET1 / book equity", "effective tax rate", "HQLA earning share (1 - cash/HQLA)",
             "wholesale spread over JIBAR (pp), solved",
             "sector NII target, 4 quarters (R bn)",
             "engine interest income vs BA120 (ratio)",
             "engine interest expense vs BA120 (ratio)",
             "engine NII vs BA120 after calibration (ratio)",
             "loan multiplier normaliser", "LCR run-off scalar, solved",
             "NSFR required-funding scalar, solved"),
    value = c(k_rw, 100 * rwa_other, cet1_share, tax, hq, ws,
              nii_target / 1e6, fl[["inc"]] / inc_target, fl[["ex"]] / exp_target,
              fl[["nii"]] / nii_target, avg_m, liq$k_lcr, liq$k_nsfr))
  list(chars = chars, rwa_other = rwa_other, cet1_share = cet1_share, tax = tax,
       hqla_scale = hq, wholesale_spread = ws, wholesale_spread_raw = ws_raw, date = date, checks = checks,
       window = q4$date, flows = list(engine = fl, targets = c(inc = inc_target, ex = exp_target,
                                                                nii = nii_target)),
       sector_bs = bal, k_rw = k_rw, liq = liq)
}

#' Line characteristics for one entity: calibrated credit parameters plus the
#' entity's depositor-weighted liquidity factors and the sector scalars
#' @param cal Output of [calibrate_to_sector()].
#' @param depmat Deposit matrices (`entity, date, class, col, value`).
#' @param entity "Sector" or "Population" (NGFB takes the six-bank pool's
#'   depositor mix within each behavioural category).
#' @param date Date of the depositor mix.
#' @return Line characteristics data frame.
#' @export
entity_chars <- function(cal, depmat, entity, date) {
  lf <- deposit_liquidity_factors(depmat[depmat$entity == entity, ], date)
  liquidity_chars(cal$chars, lf, cal$liq$k_lcr %||% 1, cal$liq$k_nsfr %||% 1)
}
