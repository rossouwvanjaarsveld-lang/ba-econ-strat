# =============================================================================
#  monthly.R -- the monthly version of the data layer
#
#  The macro drivers keep the names of the quarterly panel, so the Stage 1
#  target registry (and its economic signs) applies unchanged; only the
#  windows change with the frequency:
#
#                         quarterly panel        monthly panel
#    d_repo, d_gov10y,    change over 1 quarter  change over 3 months
#      d_unemp
#    d4_repo, d4_unemp    change over 4 quarters change over 12 months
#    *_yy growth          4-quarter log change   12-month log change
#    lags                 0-4 quarters           0, 1, 2, 3, 6, 9, 12 months
#
#  Real GDP, the unemployment rate and house prices are quarterly; they are
#  disaggregated to months with the Kalman filter (see disaggregate.R).
# =============================================================================

#' Derived macro drivers for a monthly or quarterly frame
#'
#' Recomputes every derived driver from the core series so that a history, a
#' scenario path or a shocked path is internally consistent (a repo shock
#' moves prime, JIBAR, the real rate, the changes and the partial sums).
#' @param m Data frame ordered by `date` with at least `repo`, `gov10y`,
#'   `cpi_yy`, `unemp`, and optionally `jibar3m`, `gdp_yy`.
#' @param freq `"month"` or `"quarter"` (sets the change windows).
#' @param jib_spread JIBAR-repo spread used where `jibar3m` is missing.
#' @param repo_start Optional `c(repo_pos, repo_neg)` at the first row (to
#'   continue partial sums from a history).
#' @return `m` with derived columns added or replaced.
#' @export
derive_drivers <- function(m, freq = c("month", "quarter"), jib_spread = 0.35, repo_start = NULL) {
  freq <- match.arg(freq)
  k1 <- if (freq == "month") 3L else 1L
  k4 <- if (freq == "month") 12L else 4L
  chg <- function(x, k) x - lag_vec(x, k)
  if (is.null(m$prime)) m$prime <- NA_real_
  m$prime <- ifelse(is.finite(m$prime), m$prime, m$repo + 3.5)
  if (is.null(m$jibar3m)) m$jibar3m <- NA_real_
  m$jibar3m <- ifelse(is.finite(m$jibar3m), m$jibar3m, m$repo + jib_spread)
  m$slope <- m$gov10y - m$jibar3m
  m$d_repo <- chg(m$repo, k1)
  m$d4_repo <- chg(m$repo, k4)
  m$d_gov10y <- chg(m$gov10y, k1)
  m$real_repo <- m$repo - m$cpi_yy
  m$d_unemp <- chg(m$unemp, k1)
  m$d4_unemp <- chg(m$unemp, k4)
  m$d_repo_pos <- pmax(m$d_repo, 0); m$d_repo_neg <- pmin(m$d_repo, 0)
  dr <- c(0, diff(m$repo)); dr[!is.finite(dr)] <- 0
  p0 <- if (is.null(repo_start)) c(0, 0) else repo_start
  m$repo_pos <- p0[1] + cumsum(pmax(dr, 0))
  m$repo_neg <- p0[2] + cumsum(pmin(dr, 0))
  if (!is.null(m$gdp_yy)) m$gdp_neg <- pmin(m$gdp_yy, 0)
  m
}

#' SARB policy rate before the KBP series starts (March 2007)
#'
#' End-of-month repo rate, January 2005 to February 2007, from the SARB's
#' Monetary Policy Committee decisions: 7.50\% until the 50 bps cut of
#' 14 April 2005 to 7.00\%; then 50 bps hikes on 8 June, 3 August,
#' 12 October and 7 December 2006 (to 9.00\%), unchanged until June 2007. The
#' KBP series (`MMSM002E`) takes over from March 2007, and agrees with the
#' decision dates from there on.
#' @return Data frame `date` (month start), `repo`.
#' @export
repo_history_early <- function() {
  months <- seq(as.Date("2005-01-01"), as.Date("2007-02-01"), by = "month")
  r <- rep(NA_real_, length(months))
  r[months <= as.Date("2005-03-01")] <- 7.50
  r[months >= as.Date("2005-04-01") & months <= as.Date("2006-05-01")] <- 7.00
  r[months == as.Date("2006-06-01") | months == as.Date("2006-07-01")] <- 7.50
  r[months == as.Date("2006-08-01") | months == as.Date("2006-09-01")] <- 8.00
  r[months == as.Date("2006-10-01") | months == as.Date("2006-11-01")] <- 8.50
  r[months >= as.Date("2006-12-01")] <- 9.00
  data.frame(date = months, repo = r)
}

#' Monthly macro panel
#'
#' Monthly series from the SARB KBP and FRED, the quarterly ones
#' (real GDP, unemployment rate, house prices) disaggregated to months with
#' the Kalman filter, and the derived drivers of [derive_drivers()].
#' The KBP repo and prime series start in March 2007; earlier months come
#' from [repo_history_early()] (prime = repo + 3.5), and the all-share index
#' before 2007 from FRED's share-price index, so that twelve-month changes
#' exist from the start of the bank data.
#' @param kbp Tidy KBP frame ([read_kbp_cache()]).
#' @param fred Long FRED frame ([read_fred_cache()]).
#' @param from First month of the panel.
#' @param to Last month (default: the last month with a policy rate).
#' @param td_from First quarter used to estimate the disaggregation models.
#' @param gdp_indicators KBP codes of the monthly GDP indicators.
#' @param gdp_model Disaggregation model for GDP.
#' @return Data frame by month (`date` = month start) with attribute
#'   `disaggregation` (the fitted models and their diagnostics).
#' @export
build_macro_panel_monthly <- function(kbp, fred, from = "2005-01-01", to = NULL,
                                      td_from = "2000-01-01",
                                      gdp_indicators = c(coinc = "DIFN002A", mfg = "MPR0000B",
                                                         retail = "MAN5008F"),
                                      gdp_model = "chow-lin") {
  from <- month_start(from); td_from <- month_start(td_from)
  ks <- function(code) { s <- kbp[kbp$series == code, c("date", "value")]; s$date <- month_start(s$date); s }
  fs <- function(nm) { s <- fred[fred$name == nm, c("date", "value")]; s$date <- month_start(s$date); s }
  if (is.null(to)) to <- max(ks("MMSM002E")$date)
  to <- month_start(to)
  months <- seq(from, to, by = "month")
  m <- data.frame(date = months)
  put <- function(nm, s) { m[[nm]] <<- s$value[match(months, s$date)] }
  put("repo", ks("MMSM002E")); put("prime", ks("MMSM001E"))
  put("jibar3m", fs("jibar3m")); put("gov10y", fs("gov10y"))
  if (any(!is.finite(m$gov10y))) {
    g2 <- ks("CMJM004A"); fill <- g2$value[match(months, g2$date)]
    m$gov10y[!is.finite(m$gov10y)] <- fill[!is.finite(m$gov10y)]
  }
  put("cpi_yy", ks("CPI1000F"))
  put("usdzar", ks("EXCB135M")); put("alsi", ks("JLE2001M"))
  put("coinc", ks("DIFN002A")); put("lead", ks("DIFN003A"))
  put("share_px", fs("share_px"))
  ## policy rate before March 2007: the MPC decision record
  eh <- repo_history_early()
  miss <- !is.finite(m$repo) & m$date %in% eh$date
  m$repo[miss] <- eh$repo[match(m$date[miss], eh$date)]
  m$prime[!is.finite(m$prime)] <- m$repo[!is.finite(m$prime)] + 3.5
  ## carry the last observation of slow-publishing monthly series forward
  m <- locf_tail(m, c("jibar3m", "gov10y", "cpi_yy", "usdzar", "alsi", "share_px", "coinc", "lead"))
  ## twelve-month growth rates (log, percent)
  g12 <- function(x) 100 * (log(x) - lag_vec(log(x), 12L))
  m$zar_yy <- g12(m$usdzar)
  alsi_g <- g12(m$alsi); sh_g <- g12(m$share_px)
  m$alsi_yy <- ifelse(is.finite(alsi_g), alsi_g, sh_g)
  m$coinc_yy <- g12(m$coinc); m$lead_yy <- g12(m$lead)

  ## quarterly series -> monthly (Kalman filter), estimated from td_from
  tdm <- seq(td_from, to, by = "month")
  ind <- data.frame(date = tdm)
  for (nm in names(gdp_indicators)) { s <- ks(gdp_indicators[[nm]]); ind[[nm]] <- s$value[match(tdm, s$date)] }
  gq <- fs("gdp_real")
  gfit <- td_gdp_monthly(gq, ind, from = td_from, to = to, model = gdp_model)
  gm <- gfit$monthly
  g12m <- function(d, v) { x <- 100 * (log(v) - lag_vec(log(v), 12L)); x[match(months, d)] }
  m$gdp_m <- gm$gdp_m[match(months, gm$date)]
  m$gdp_yy <- g12m(gm$date, gm$gdp_m)
  m$gdp_3m <- (400 * (log(gm$gdp_m) - lag_vec(log(gm$gdp_m), 3L)))[match(months, gm$date)]
  un <- td_smooth_monthly(fs("unemp_rate"), from = td_from, to = to, log = FALSE)
  m$unemp <- un$value[match(months, un$date)]
  hp <- td_smooth_monthly(fs("house_px_nominal"), from = td_from, to = to, log = TRUE)
  m$hpi <- hp$value[match(months, hp$date)]
  m$hpi_yy <- g12m(hp$date, hp$value)
  m <- derive_drivers(m, "month", jib_spread = stats::median(m$jibar3m - m$repo, na.rm = TRUE))
  attr(m, "disaggregation") <- list(
    gdp = gfit[c("beta", "phi", "sigma2", "loglik", "aic", "model", "indicators", "n_quarters")],
    gdp_monthly = gfit$monthly,
    unemp = attr(un, "fit"), hpi = attr(hp, "fit"),
    repo_fill = months[miss],
    last_quarter = list(gdp = max(gq$date[is.finite(gq$value)]),
                        unemp = max(fs("unemp_rate")$date), hpi = max(fs("house_px_nominal")$date)))
  m
}

#' Monthly bank panel from BA900 product lines
#'
#' The monthly analogue of [bank_panel_quarterly()]: every month is kept,
#' keyed on its first day.
#' @param lines The `lines` element of [ba900_lines()] (monthly periods).
#' @param patterns Institution patterns.
#' @return Data frame `date, key, <lines...>`.
#' @export
bank_panel_monthly <- function(lines, patterns = default_ba900_institutions()) {
  lines$date <- month_start(lines$period)
  keys <- institution_keys(lines, patterns)
  q <- merge(lines, keys, by = "inst_id")
  agg <- stats::aggregate(list(value = q$value), list(date = q$date, key = q$key, line = q$line), sum)
  w <- stats::reshape(agg, idvar = c("date", "key"), timevar = "line", direction = "wide")
  names(w) <- sub("^value\\.", "", names(w))
  for (v in setdiff(names(w), c("date", "key"))) w[[v]][is.na(w[[v]])] <- 0
  w[order(w$key, w$date), ]
}

#' Monthly sector P&L panel from BA100 stocks and BA120 flows
#'
#' Interest rates (yields, deposit costs, the margin) are annualised with the
#' month's day count, `100 * flow / average stock * 365 / days`, because
#' interest accrues by the day; credit losses, fees and costs are monthly
#' accounting flows, annualised as `1200 * flow / average stock`. The
#' average stock is the mean of the opening and closing month-end stocks.
#' [months_to_quarters()] with `day_weights()` maps monthly rates back to the
#' quarterly definition of [sector_pnl_panel()]. BA120 (income) is monthly;
#' when BA100 (the balance
#' sheet) is available only at quarter ends, the month-end stocks in between
#' are interpolated log-linearly: stocks move slowly, so the monthly
#' information is in the flows, and the interpolation moves a monthly rate
#' by a few hundredths of a percentage point at most.
#' @param ba100m Long BA100 frame (every month, or quarter ends only).
#' @param ba120 Long BA120 frame.
#' @return Data frame by month (`date` = month start), attribute
#'   `interpolated_stocks` (logical).
#' @export
sector_pnl_panel_monthly <- function(ba100m, ba120) {
  s <- extract_concepts(ba100m, ba100_concepts(), col = "3")
  f <- extract_concepts(ba120, ba120_concepts(), col = "3")
  s$date <- month_start(s$period); f$date <- month_start(f$period)
  wide <- function(d) {
    w <- stats::reshape(d[, c("date", "concept", "value")], idvar = "date", timevar = "concept",
                        direction = "wide")
    names(w) <- sub("^value\\.", "", names(w)); w
  }
  sw <- wide(s); sw <- sw[order(sw$date), ]
  interp <- !all(diff(as.numeric(sw$date)) %in% 28:31)
  if (interp) {
    months <- seq(min(sw$date), max(sw$date), by = "month")
    full <- data.frame(date = months)
    for (v in setdiff(names(sw), "date")) {
      x <- sw[[v]]; ok <- is.finite(x)
      pos <- ok & x > 0
      full[[v]] <- if (sum(pos) >= 2 && all(x[ok] > 0))
        exp(stats::approx(as.numeric(sw$date[pos]), log(x[pos]), xout = as.numeric(months))$y)
      else if (sum(ok) >= 2) stats::approx(as.numeric(sw$date[ok]), x[ok], xout = as.numeric(months))$y
      else NA_real_
    }
    sw <- full
  }
  m <- merge(sw, wide(f), by = "date", all = TRUE)
  m <- m[order(m$date), ]
  ## consecutive months only for averages
  prev_ok <- c(FALSE, diff(as.numeric(m$date)) %in% 28:31)
  avg <- function(v) if (v %in% names(m)) ifelse(prev_ok, (m[[v]] + lag_vec(m[[v]], 1L)) / 2, NA_real_) else NA_real_
  rate <- function(flow, stock) if (flow %in% names(m)) dc * m[[flow]] / stock else NA_real_
  gl <- c("homeloans", "commercial_mortgages", "credit_cards", "lease_instalment", "overdrafts",
          "pref_shares", "trade_bills", "term_loans", "factoring", "resale", "other_loans")
  m$gross_loans <- rowSums(m[, intersect(gl, names(m)), drop = FALSE], na.rm = TRUE)
  m$deposits <- rowSums(m[, intersect(c("dep_current", "dep_savings", "dep_call", "dep_fixed_notice",
                                        "dep_ncd", "dep_other", "dep_repo"), names(m)), drop = FALSE],
                        na.rm = TRUE)
  m$iea <- rowSums(m[, intersect(c("cash_cb", "st_securities", "gov_securities",
                                   "other_dated_securities", "pledged_assets"), names(m)),
                     drop = FALSE], na.rm = TRUE) + m$gross_loans - m$loan_impairments
  ## interest accrues by the day: a month's interest is annualised with its
  ## own day count (365 / days), so February is not read as a fall in rates
  m$days <- days_in_month(m$date)
  dc <- 36500 / m$days
  m$avg_loans <- avg("gross_loans"); m$avg_iea <- avg("iea"); m$avg_assets <- avg("total_assets")
  m$avg_equity <- avg("equity_ordinary"); m$avg_deposits <- avg("deposits")
  m$y_homeloans  <- rate("ii_homeloans", avg("homeloans"))
  m$y_commercial <- rate("ii_commercial_mortgages", avg("commercial_mortgages"))
  m$y_cards      <- rate("ii_credit_cards", avg("credit_cards"))
  m$y_vaf        <- rate("ii_lease_instalment", avg("lease_instalment"))
  m$y_overdrafts <- rate("ii_overdrafts", avg("overdrafts"))
  m$y_term_loans <- rate("ii_term_loans", avg("term_loans"))
  m$y_loans      <- rate("ii_loans", m$avg_loans)
  m$c_current    <- rate("ie_current", avg("dep_current"))
  m$c_savings    <- rate("ie_savings", avg("dep_savings"))
  m$c_fixed_notice <- rate("ie_fixed_notice", avg("dep_fixed_notice"))
  m$c_call       <- rate("ie_term_other", avg("dep_call"))
  m$c_ncd        <- rate("ie_ncd", avg("dep_ncd"))
  m$c_deposits   <- rate("ie_deposits", m$avg_deposits)
  m$nim          <- dc * m$nii / m$avg_iea
  m$clr          <- 1200 * m$credit_losses / m$avg_loans
  m$nir_assets   <- 1200 * m$nir / m$avg_assets
  m$opex_assets  <- 1200 * (m$opex + m$indirect_tax) / m$avg_assets
  attr(m, "interpolated_stocks") <- interp
  m
}

#' Monthly Stage 1 panel (macro drivers + sector targets)
#'
#' Volumes are annualised month-on-month log growth (`1200 dlog`), so the
#' mean of a quarter's three months equals the quarterly target of
#' [stage1_panel()] exactly; P&L ratios are the monthly annualised rates of
#' [sector_pnl_panel_monthly()]. Month-of-year dummies (`sm02`..`sm12`) and
#' COVID dummies (April to September 2020) are added as controls.
#' @param gp_m Monthly group panel with a "Sector" entity.
#' @param macro_m Monthly macro panel ([build_macro_panel_monthly()]).
#' @param spnl_m Monthly sector P&L panel (may be `NULL`).
#' @param end Last month of the estimation sample.
#' @return Data frame by month.
#' @export
stage1_panel_monthly <- function(gp_m, macro_m, spnl_m = NULL, end = NULL) {
  gp_m <- add_composites(gp_m)
  sec <- gp_m[gp_m$entity == "Sector", ]
  sec <- sec[order(sec$date), ]
  out <- data.frame(date = sec$date)
  cons <- c(FALSE, diff(as.numeric(sec$date)) %in% 28:31)
  for (p in c(model_products(), "total_assets", "loans_total", "dep_retail", "dep_corporate",
              "dep_financial", "overdrafts", "corporate_loans"))
    if (p %in% names(sec)) out[[paste0("g_", p)]] <- ifelse(cons, 1200 * c(NA, diff(log(sec[[p]]))), NA_real_)
  out <- merge(out, macro_m, by = "date", all.x = TRUE)
  if (!is.null(spnl_m)) {
    keep <- c("date", "nim", "clr", "nir_assets", "opex_assets", "y_homeloans", "y_commercial", "y_vaf",
              "y_cards", "y_overdrafts", "y_term_loans", "y_loans", "c_current", "c_savings", "c_call",
              "c_fixed_notice", "c_ncd", "c_deposits")
    out <- merge(out, spnl_m[, intersect(keep, names(spnl_m))], by = "date", all.x = TRUE)
    out$s_homeloans <- out$y_homeloans - out$prime
    out$s_commercial <- out$y_commercial - out$prime
    out$s_vaf <- out$y_vaf - out$prime
    out$s_cards <- out$y_cards - out$prime
    out$s_overdrafts <- out$y_overdrafts - out$prime
    out$s_term_loans <- out$y_term_loans - out$jibar3m
  }
  out <- cbind(out, month_dummies(out$date))
  ## COVID: the 2020 Q2 and Q3 dummies of the quarterly panel, month by month
  out$covid_q2 <- as.numeric(out$date >= as.Date("2020-04-01") & out$date <= as.Date("2020-06-01"))
  out$covid_q3 <- as.numeric(out$date >= as.Date("2020-07-01") & out$date <= as.Date("2020-09-01"))
  if (!is.null(end)) out <- out[out$date <= month_start(end), ]
  out[order(out$date), ]
}
