# =============================================================================
#  panel_macro.R -- the quarterly macro panel (drivers of Stage 1)
#
#  Sources, in order of preference:
#    SARB KBP (MonthlyIndicatorsAll): repo, prime, long government bond yield,
#      CPI inflation, R/USD, JSE all-share, coincident and leading indicators
#    FRED: real/nominal GDP, unemployment rate, residential property prices
#      (BIS), 3m interbank rate, 10y yield (OECD MEI), household debt/GDP (BIS)
#
#  Monthly series are averaged within the quarter; quarters are keyed on the
#  quarter-end date. Growth rates are in percent: q/q annualised log changes
#  (suffix _qq) or year-on-year log changes (suffix _yy).
# =============================================================================

#' Default SARB KBP codes for the macro panel
#' @return Named character vector (panel name -> KBP code).
#' @export
kbp_macro_codes <- function() {
  c(repo    = "MMSM002E",   # repurchase rate, monthly average
    prime   = "MMSM001E",   # prime overdraft rate
    gov_lt  = "CMJM004A",   # yield on long-term government bonds
    cpi_yy_kbp = "CPI1000F",# CPI, 12-month % change
    usdzar  = "EXCB135M",   # rand per US dollar
    alsi    = "JLE2001M",   # FTSE/JSE all share
    coinc   = "DIFN002A",   # coincident business-cycle indicator
    lead    = "DIFN003A",   # leading business-cycle indicator
    credit_pvt = "MON0023A")# credit extension to the domestic private sector
}

#' Aggregate a monthly series to quarterly averages
#' @param date Date vector (any day in the month).
#' @param value Numeric vector.
#' @param min_months Minimum months observed for a quarter to count.
#' @return Data frame `date` (quarter end), `value`.
#' @export
to_quarterly <- function(date, value, min_months = 2L) {
  q <- quarter_end(date)
  ok <- is.finite(value)
  a <- stats::aggregate(list(value = value[ok]), list(date = q[ok]), mean)
  n <- stats::aggregate(list(n = value[ok]), list(date = q[ok]), length)
  a <- merge(a, n, by = "date")
  a <- a[a$n >= min_months, c("date", "value")]
  a[order(a$date), , drop = FALSE]
}

.qser <- function(df, name, min_months = 2L) {
  if (is.null(df) || !nrow(df)) return(NULL)
  q <- to_quarterly(df$date, df$value, min_months = min_months)
  names(q)[2] <- name
  q
}

#' Annualised quarter-on-quarter log growth in percent
#' @param x Numeric level series (quarterly).
#' @return Numeric vector.
#' @export
growth_qq <- function(x) 400 * c(NA, diff(log(x)))

#' Year-on-year log growth in percent (quarterly data)
#' @param x Numeric level series (quarterly).
#' @return Numeric vector.
#' @export
growth_yy <- function(x) 100 * (log(x) - lag_vec(log(x), 4L))

#' Build the quarterly macro panel
#'
#' @param kbp Tidy KBP frame from [read_kbp_cache()].
#' @param fred Long FRED frame from [read_fred_cache()] (may be `NULL`).
#' @param from First quarter kept.
#' @return Data frame, one row per quarter (quarter-end `date`), with levels
#'   and transformed drivers. Attribute `sources` documents each column.
#' @export
build_macro_panel <- function(kbp, fred = NULL, from = "2005-03-31") {
  codes <- kbp_macro_codes()
  parts <- list()
  src <- character(0)
  for (nm in names(codes)) {
    s <- kbp[kbp$series == codes[[nm]], c("date", "value")]
    if (nrow(s)) { parts[[nm]] <- .qser(s, nm); src[nm] <- paste0("SARB ", codes[[nm]]) }
  }
  if (!is.null(fred) && nrow(fred)) {
    for (nm in unique(fred$name)) {
      s <- fred[fred$name == nm, c("date", "value")]
      parts[[paste0("fred_", nm)]] <- .qser(s, paste0("fred_", nm), min_months = 1L)
      src[paste0("fred_", nm)] <- paste0("FRED ", unique(fred$code[fred$name == nm]))
    }
  }
  parts <- Filter(Negate(is.null), parts)
  m <- Reduce(function(a, b) merge(a, b, by = "date", all = TRUE), parts)
  m <- m[m$date >= as.Date(from), , drop = FALSE]
  m <- m[order(m$date), , drop = FALSE]
  g <- function(v) if (v %in% names(m)) m[[v]] else rep(NA_real_, nrow(m))

  ## rates (percent)
  m$repo   <- g("repo")
  m$prime  <- ifelse(is.finite(g("prime")), g("prime"), m$repo + 3.5)
  m$jibar3m <- ifelse(is.finite(g("fred_jibar3m")), g("fred_jibar3m"), m$repo + 0.35)
  m$gov10y <- ifelse(is.finite(g("fred_gov10y")), g("fred_gov10y"), g("gov_lt"))
  m$slope  <- m$gov10y - m$jibar3m
  m$d_repo <- c(NA, diff(m$repo))
  m$d4_repo <- m$repo - lag_vec(m$repo, 4L)
  m$d_gov10y <- c(NA, diff(m$gov10y))
  ## partial sums of policy-rate increases and decreases since the start of
  ## the panel (repo = repo_0 + repo_pos + repo_neg), for asymmetric
  ## pass-through (Shin, Yu and Greenwood-Nimmo, 2014)
  m <- asym_decompose(m, "repo", "partial_sum")
  m <- asym_decompose(m, "d_repo", "split")        # rises and falls of the quarterly change

  ## activity: real GDP if available, else the coincident indicator
  gdp_real <- if ("fred_gdp_real" %in% names(m)) g("fred_gdp_real") else rep(NA_real_, nrow(m))
  m$gdp_real <- gdp_real
  m$gdp_yy <- growth_yy(gdp_real)
  m$gdp_neg <- pmin(m$gdp_yy, 0)                  # contraction only (convex credit-loss check)
  m$gdp_qq <- growth_qq(gdp_real)
  m$coinc_yy <- growth_yy(g("coinc"))
  m$lead_yy  <- growth_yy(g("lead"))
  m$gdp_nom_yy <- growth_yy(g("fred_gdp_nominal"))

  ## prices
  m$cpi_yy <- ifelse(is.finite(g("cpi_yy_kbp")), g("cpi_yy_kbp"), growth_yy(g("fred_cpi_index")))
  m$hpi_yy <- growth_yy(g("fred_house_px_nominal"))
  m$hpi_real_yy <- growth_yy(g("fred_house_px_real"))
  m$zar_yy <- growth_yy(ifelse(is.finite(g("usdzar")), g("usdzar"), g("fred_usdzar")))
  m$alsi_yy <- growth_yy(ifelse(is.finite(g("alsi")), g("alsi"), g("fred_share_px")))
  m$real_repo <- m$repo - m$cpi_yy

  ## labour market
  m$unemp <- g("fred_unemp_rate")
  m$d_unemp <- c(NA, diff(m$unemp))
  m$d4_unemp <- m$unemp - lag_vec(m$unemp, 4L)

  ## leverage
  m$hh_debt_gdp <- g("fred_hh_debt_gdp")
  m$credit_pvt_yy <- growth_yy(g("credit_pvt"))

  attr(m, "sources") <- src
  m
}
