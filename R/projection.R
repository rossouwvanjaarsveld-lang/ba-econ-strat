# =============================================================================
#  projection.R -- macro scenario -> sector (Stage 1) -> NGFB (Stage 2) ->
#                  engine inputs -> P&L, capital and ROE
# =============================================================================

#' How each engine line is priced from Stage 1 outputs
#'
#' `ref` + projected sector spread (`spread_target`) or a fixed spread for
#' lines without a sector yield in BA120 (personal loans, treasury and
#' wholesale lines); deposit categories take a weighted blend of projected
#' sector costs by product.
#'
#' Loans and deposits are priced from BA120 product series, and their BA900
#' balances match BA100's product balances closely. Treasury assets and
#' wholesale funding carry assumed spreads over their reference rates; the
#' wholesale spread is solved by [calibrate_to_sector()] so that the engine's
#' sector NII matches BA120.
#' @return Named list of pricing rules.
#' @export
line_pricing_map <- function() {
  p <- list(
    mortgages_res = list(ref = "prime", spread_target = "s_homeloans"),
    mortgages_com = list(ref = "prime", spread_target = "s_commercial"),
    vaf = list(ref = "prime", spread_target = "s_vaf"),
    cards = list(ref = "prime", spread_target = "s_cards"),
    personal_other = list(ref = "prime", spread_fixed = 8.0),
    corporate_loans = list(ref = "jibar3m", spread_target = "s_term_loans"),
    overdrafts = list(ref = "prime", spread_target = "s_overdrafts"),
    interbank = list(ref = "jibar3m", spread_fixed = -0.10),
    reverse_repo = list(ref = "repo", spread_fixed = 0.15),
    hqla = list(ref = "tbill", spread_fixed = 0, scale = 0.80),
    other_securities = list(ref = "jibar3m", spread_fixed = 0.50),
    beh_operational = list(cost = c(c_current = 0.5, c_call = 0.5)),
    beh_savings = list(cost = c(c_savings = 1)),
    beh_notice = list(cost = c(c_call = 1)),
    beh_term = list(cost = c(c_fixed_notice = 0.8, c_ncd = 0.2)),
    repo_funding = list(ref = "repo", spread_fixed = 0.10),
    wholesale_other = list(ref = "jibar3m", spread_fixed = 1.00))
  p
}

#' Rate of one priced line in one quarter (percent)
#' @param rule One element of [line_pricing_map()].
#' @param row One-row data frame (or list) with reference rates, spreads and
#'   costs.
#' @return Numeric rate in percent.
#' @export
line_rate <- function(rule, row) {
  if (!is.null(rule$cost)) {
    v <- 0
    for (k in names(rule$cost)) v <- v + rule$cost[[k]] * row[[k]]
    return(v)
  }
  sp <- if (!is.null(rule$spread_target)) row[[rule$spread_target]] else (rule$spread_fixed %||% 0)
  row[[rule$ref]] * (rule$scale %||% 1) + sp
}

#' Default line characteristics: risk weights, loss multipliers, liquidity factors
#'
#' Risk weights are the sector's (largely AIRB) risk-weighted exposure per
#' unit of exposure by Basel asset class in BA200 (2025Q2): residential
#' mortgages 23\%, revolving retail 47\%, other retail 52\%, SME retail 46\%,
#' corporate 36\%, SME corporate 48\%, banks 11\%, other 12\%. Non-credit RWA is
#' added separately (BA700 total less BA200 credit RWA).
#' Credit-loss multipliers are RELATIVE loss rates (sector average = 1 after
#' normalisation); BA200 publishes no losses by product, so the ordering is
#' an external calibration and is reported as an assumption.
#'
#' Liquidity factors are simplified Basel III factors. `runoff` is the 30-day
#' outflow rate on liabilities and the 30-day inflow rate on assets
#' (contractual repayments inside 30 days at 50\% for loans, half of interbank
#' placements, reverse repos mostly against Level 1 collateral). The deposit
#' lines' `runoff` and `asf` defaults here are overwritten by depositor-
#' weighted factors from BA900 ([deposit_liquidity_factors()]). `hqla` = 1 on
#' the HQLA line and -1 on repo funding (the collateral it encumbers).
#' @return Data frame by line.
#' @export
line_characteristics <- function() {
  data.frame(
    line = c("mortgages_res", "mortgages_com", "vaf", "cards", "personal_other",
             "corporate_loans", "overdrafts", "interbank", "reverse_repo", "hqla",
             "other_securities", "other_assets", "beh_operational", "beh_savings",
             "beh_notice", "beh_term", "repo_funding", "wholesale_other",
             "other_liabilities", "equity"),
    rw = c(0.231, 0.477, 0.518, 0.472, 0.518, 0.356, 0.470, 0.108, 0.050, 0.00, 0.123, 1.00,
           rep(0, 8)),
    clr_mult = c(0.25, 0.35, 1.20, 4.00, 6.50, 0.35, 1.40, 0.02, 0.02, 0, 0, 0, rep(0, 8)),
    runoff = c(0.005, 0.010, 0.010, 0.010, 0.015, 0.050, 0, 0.50, 0.15, 0, 0, 0,
               0.10, 0.10, 0.25, 0.05, 0, 0.25, 0, 0),
    hqla = c(rep(0, 9), 1, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0),
    asf = c(rep(0, 12), 0.90, 0.90, 0.50, 0.60, 0, 0.50, 0, 1),
    rsf = c(0.65, 0.85, 0.85, 0.65, 0.85, 0.70, 0.50, 0.15, 0.10, 0.03, 0.50, 1.00, rep(0, 8)),
    earning = c(rep(TRUE, 11), FALSE, rep(TRUE, 6), FALSE, FALSE),
    stringsAsFactors = FALSE)
}

#' Assemble an engine balance sheet from balances and line characteristics
#' @param bal Data frame `line, side, balance`.
#' @param chars Line characteristics ([line_characteristics()]).
#' @param pricing Pricing map ([line_pricing_map()]) -- used for defaults.
#' @return Data frame with all [bs_columns()].
#' @export
engine_balance_sheet <- function(bal, chars = line_characteristics(), pricing = line_pricing_map()) {
  b <- merge(bal[, c("line", "side", "balance")], chars, by = "line", all.x = TRUE, sort = FALSE)
  b$ref <- vapply(b$line, function(l) pricing[[l]]$ref %||% (if (b$side[b$line == l][1] == "deposit") "deposit" else "none"), "")
  b$spread <- vapply(b$line, function(l) (pricing[[l]]$spread_fixed %||% 0) / 100, 0)
  b$alpha <- 0; b$beta <- 0
  for (v in c("rw", "clr_mult", "runoff", "hqla", "asf", "rsf")) b[[v]][is.na(b[[v]])] <- 0
  b$earning[is.na(b$earning)] <- FALSE
  b[match(bal$line, b$line), bs_columns()]
}

#' Fill trailing missing values of selected columns by carrying the last observation forward
#' @param d Data frame.
#' @param cols Columns.
#' @return `d`.
#' @export
locf_tail <- function(d, cols) {
  for (v in intersect(cols, names(d))) {
    x <- d[[v]]; ok <- which(is.finite(x))
    if (length(ok) && max(ok) < length(x)) x[(max(ok) + 1L):length(x)] <- x[max(ok)]
    d[[v]] <- x
  }
  d
}

#' Project every satellite along a scenario
#'
#' Targets whose data end before the scenario starts (the BA120/BA100 sector
#' P&L ends in 2025Q2) are first "nowcast" through the realised macro data up
#' to the jump-off, then projected along the scenario.
#' @param sats Named list of satellites.
#' @param hist Stage 1 panel (history of targets and drivers).
#' @param scen Scenario frame (macro paths).
#' @param method Projection method ("bma" or "best").
#' @param ic_window Intercept-correction window in quarters (0 = none); see
#'   [satellite_intercept_correction()].
#' @param ic_families Satellite families the correction applies to (ratio
#'   targets; volume growth rates are left uncorrected).
#' @param ic_mode `"level"` or `"equation"`; see [satellite_project()].
#' @return Data frame `date` + one column per target (scenario quarters plus
#'   any nowcast quarters), attribute `nowcast_dates`.
#' @export
project_satellites <- function(sats, hist, scen, method = "bma", ic_window = 0L,
                               ic_families = c("pnl", "credit", "pricing"), ic_mode = "level") {
  drivers <- unique(unlist(lapply(sats, function(s) c(s$spec$d, unlist(lapply(s$bma$specs, `[[`, "d"))))))
  start <- min(scen$date)
  hh <- hist[hist$date < start, , drop = FALSE]
  hh <- locf_tail(hh, drivers)
  out <- data.frame(date = scen$date)
  now <- as.Date(character(0))
  for (nm in names(sats)) {
    s <- sats[[nm]]; tg <- s$target
    last_y <- max(hh$date[is.finite(hh[[tg]])])
    bridge <- hh[hh$date > last_y, c("date", intersect(drivers, names(hh))), drop = FALSE]
    sc <- scen[, c("date", intersect(drivers, names(scen))), drop = FALSE]
    for (m in setdiff(names(bridge), names(sc))) sc[[m]] <- NA_real_
    for (m in setdiff(names(sc), names(bridge))) bridge[[m]] <- NA_real_
    path <- rbind(bridge[, names(sc)], sc)
    h <- hh[hh$date <= last_y, , drop = FALSE]
    icw <- if ((s$family %||% "") %in% ic_families) ic_window else 0L
    p <- satellite_project(s, h, path, method = method, ic_window = icw, ic_mode = ic_mode)
    if (nrow(bridge)) now <- union(now, bridge$date)
    out[[tg]] <- p$value[match(out$date, p$date)]
    attr(out, paste0("bridge_", tg)) <- p[p$date %in% bridge$date, , drop = FALSE]
  }
  attr(out, "nowcast_dates") <- sort(as.Date(now, origin = "1970-01-01"))
  out
}

#' Scenario regimes (contraction / hiking / easing / neutral)
#' @param scen Scenario frame.
#' @param hist Macro history (for the policy-rate change at the junction).
#' @return Data frame `date, regime`.
#' @export
scenario_regimes <- function(scen, hist) {
  h <- utils::tail(hist[is.finite(hist$repo) & hist$date < min(scen$date), c("date", "repo", "gdp_yy")], 4L)
  z <- rbind(h, scen[, c("date", "repo", "gdp_yy")])
  z$regime <- classify_regimes(z)
  z[z$date %in% scen$date, c("date", "regime")]
}

#' Build all engine inputs for one scenario
#'
#' @param proj Output of [project_satellites()] (volume and P&L satellites).
#' @param scen Scenario frame.
#' @param regimes Scenario regimes.
#' @param share_params NGFB share-dynamics parameters (product x param);
#'   `NULL` gives sector-average growth (used for the sector run).
#' @param mean_g Named sector mean growth per product (share-model centring).
#' @param drift Include NGFB's structural share drift.
#' @param pricing Pricing map.
#' @param ln_map Line -> modelled product map ([line_driver_map()]).
#' @param overlays Optional named list of additive overlays (percentage points)
#'   on projected targets, e.g. `list(clr = 0.3)` for a judgemental top-up.
#' @return List: `growth`, `rates`, `credit`, `ratios`, `line_rates`.
#' @export
engine_inputs <- function(proj, scen, regimes, share_params = NULL, mean_g = NULL,
                          drift = FALSE, pricing = line_pricing_map(),
                          ln_map = line_driver_map(), overlays = list()) {
  d <- proj[proj$date %in% scen$date, , drop = FALSE]
  d <- d[match(scen$date, d$date), , drop = FALSE]
  for (v in names(overlays)) if (v %in% names(d)) d[[v]] <- d[[v]] + overlays[[v]]
  reg <- regimes$regime[match(scen$date, regimes$date)]
  ## volumes: sector growth (+ NGFB share growth) -> quarterly growth factors
  growth <- data.frame(date = scen$date)
  for (ln in names(ln_map)) {
    p <- ln_map[[ln]]; g <- d[[paste0("g_", p)]]
    if (is.null(g)) next
    ds <- 0
    if (!is.null(share_params) && p %in% rownames(share_params)) {
      ds <- share_growth(share_params[p, ], reg, g, mean_g[[p]] %||% 0, drift = drift)
      ds[!is.finite(ds)] <- 0
    }
    growth[[ln]] <- exp((g + ds) / 400)
  }
  rates <- data.frame(date = scen$date, repo = scen$repo, jibar = scen$jibar3m,
                      jibar3m = scen$jibar3m, prime = scen$prime, tbill = scen$tbill,
                      gov10y = scen$gov10y, swap5y = scen$swap5y)
  line_rates <- data.frame(date = scen$date)
  rows <- cbind(d[, setdiff(names(d), names(rates)), drop = FALSE], rates)
  for (ln in names(pricing))
    line_rates[[ln]] <- vapply(seq_len(nrow(rows)), function(i) line_rate(pricing[[ln]], rows[i, ]), 0)
  list(growth = growth, rates = rates,
       credit = data.frame(date = scen$date, clr = pmax(d$clr, 0)),
       ratios = data.frame(date = scen$date, nir_assets = d$nir_assets, opex_assets = d$opex_assets),
       line_rates = line_rates, regimes = reg)
}
