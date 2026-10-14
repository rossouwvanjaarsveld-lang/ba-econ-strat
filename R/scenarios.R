# =============================================================================
#  scenarios.R -- macro scenario construction
#
#  A scenario is a quarterly data frame of macro paths in the same units as
#  the macro panel. Scenarios are built as a BASELINE (persistence from the
#  latest observation towards long-run anchors, optionally with an explicit
#  policy-rate path) plus DEVIATION paths (ramp to a peak, hold, then decay).
#  Derived variables (prime, JIBAR, slope, changes, real rate) are recomputed
#  from the core paths by scenario_derive(), so a scenario can never be
#  internally inconsistent (e.g. a repo shock without a prime shock).
# =============================================================================

#' Core scenario variables
#' @return Character vector of the macro variables a scenario specifies.
#' @export
scenario_core_vars <- function() {
  c("repo", "gov10y", "cpi_yy", "gdp_yy", "unemp", "hpi_yy", "zar_yy", "alsi_yy")
}

#' Deviation path: ramp to a peak, hold, then decay geometrically
#' @param n Number of quarters.
#' @param peak Peak deviation (units of the variable).
#' @param ramp Quarters to reach the peak (linear).
#' @param hold Quarters at the peak.
#' @param half_life Half-life (quarters) of the decay after the hold.
#' @param delay Quarters of zero deviation before the ramp starts.
#' @return Numeric vector of length `n`.
#' @export
shock_path <- function(n, peak, ramp = 2, hold = 2, half_life = 4, delay = 0) {
  v <- numeric(n)
  for (t in seq_len(n)) {
    s <- t - delay
    v[t] <- if (s <= 0) 0 else if (s <= ramp) peak * s / ramp
    else if (s <= ramp + hold) peak
    else peak * 0.5^((s - ramp - hold) / half_life)
  }
  v
}

#' Next n quarter-end dates after a given quarter end
#' @param last Last observed quarter-end date.
#' @param n Number of quarters.
#' @return Date vector.
#' @export
next_quarters <- function(last, n) {
  last <- as.Date(last)
  y <- as.integer(format(last, "%Y")); m <- as.integer(format(last, "%m"))
  out <- as.Date(character(0))
  for (i in seq_len(n)) {
    m <- m + 3L; if (m > 12L) { m <- m - 12L; y <- y + 1L }
    out <- c(out, quarter_end(as.Date(sprintf("%04d-%02d-01", y, m))))
  }
  out
}

#' Recompute derived macro variables from core paths
#'
#' @param s Scenario frame with at least the core variables.
#' @param hist Macro history (used for lags at the junction and for the
#'   JIBAR-repo and T-bill spreads).
#' @return `s` with prime, jibar3m, tbill, swap5y, slope, d_repo, d_gov10y,
#'   d_unemp, d4_unemp, real_repo, gdp_qq, coinc_yy (proxy) added.
#' @export
scenario_derive <- function(s, hist) {
  h <- utils::tail(hist[is.finite(hist$repo), , drop = FALSE], 8L)
  jib_spread <- stats::median(h$jibar3m - h$repo, na.rm = TRUE)
  if (!is.finite(jib_spread)) jib_spread <- 0.35
  s$prime   <- s$repo + 3.5
  s$jibar3m <- s$repo + jib_spread
  s$jibar   <- s$jibar3m
  s$tbill   <- s$jibar3m - 0.15
  s$slope   <- s$gov10y - s$jibar3m
  s$swap5y  <- s$jibar3m + 0.55 * s$slope           # 5y swap ~ inside the 10y bond curve
  # junction values: quarterly macro series are published with different lags
  # (unemployment and GDP a quarter behind the policy rate), so the last few
  # history rows are filled forward before differencing
  last <- utils::tail(locf_tail(hist, c("repo", "gov10y", "unemp", "cpi_yy", "gdp_yy")), 4L)
  cat_prev <- function(v, k = 1L) c(utils::tail(last[[v]], k), s[[v]])
  s$d_repo   <- diff(cat_prev("repo"))
  s$d_repo_pos <- pmax(s$d_repo, 0); s$d_repo_neg <- pmin(s$d_repo, 0)
  r4 <- c(utils::tail(last$repo, 4L), s$repo)
  s$d4_repo  <- r4[-(1:4)] - r4[seq_len(nrow(s))]
  s$d_gov10y <- diff(cat_prev("gov10y"))
  s$d_unemp  <- diff(cat_prev("unemp"))
  u4 <- c(utils::tail(last$unemp, 4L), s$unemp)
  s$d4_unemp <- u4[-(1:4)] - u4[seq_len(nrow(s))]
  s$real_repo <- s$repo - s$cpi_yy
  s$gdp_qq <- s$gdp_yy                                 # steady-state approximation
  s$coinc_yy <- s$gdp_yy
  s$gdp_neg <- pmin(s$gdp_yy, 0)
  ## partial sums of repo increases and decreases carry on from history
  if (all(c("repo_pos", "repo_neg") %in% names(hist))) {
    hp <- utils::tail(hist[is.finite(hist$repo_pos) & is.finite(hist$repo), c("repo", "repo_pos", "repo_neg")], 1L)
    dr <- diff(c(hp$repo, s$repo))
    s$repo_pos <- hp$repo_pos + cumsum(pmax(dr, 0))
    s$repo_neg <- hp$repo_neg + cumsum(pmin(dr, 0))
  }
  s
}

#' Baseline scenario: persistence from the jump-off towards long-run anchors
#'
#' @param hist Quarterly macro panel (see [build_macro_panel()]).
#' @param n Horizon in quarters.
#' @param anchors Named list of long-run values for the core variables.
#' @param half_life Convergence half-life in quarters (named list or scalar).
#' @param repo_path Optional explicit policy-rate path (length `n`), percent.
#' @param jump_off Optional named list overriding the last observed values
#'   (e.g. a policy decision announced after the data cut-off).
#' @param paths Optional named list of explicit paths for core variables.
#' @return Scenario data frame (`date` + core + derived variables).
#' @export
scenario_baseline <- function(hist, n = 12L,
                              anchors = list(repo = 6.75, gov10y = 9.5, cpi_yy = 3.0,
                                             gdp_yy = 1.5, unemp = 32.0, hpi_yy = 4.0,
                                             zar_yy = 2.0, alsi_yy = 9.0),
                              half_life = 4, repo_path = NULL, jump_off = list(),
                              paths = list()) {
  last_obs <- function(v) {
    x <- hist[[v]]; x <- x[is.finite(x)]
    if (!length(x)) NA_real_ else utils::tail(x, 1)
  }
  last_date <- max(hist$date[is.finite(hist$repo)])
  dates <- next_quarters(last_date, n)
  s <- data.frame(date = dates)
  for (v in scenario_core_vars()) {
    x0 <- jump_off[[v]] %||% last_obs(v)
    if (!is.finite(x0)) x0 <- anchors[[v]]
    hl <- if (is.list(half_life)) (half_life[[v]] %||% 4) else half_life
    w <- 0.5^(seq_len(n) / hl)
    s[[v]] <- anchors[[v]] + (x0 - anchors[[v]]) * w
  }
  if (!is.null(repo_path)) s$repo <- repo_path[seq_len(n)]
  for (v in names(paths)) s[[v]] <- paths[[v]][seq_len(n)]
  attr(s, "jump_off_date") <- last_date
  scenario_derive(s, hist)
}

#' Apply deviation paths to a baseline
#' @param base Baseline scenario.
#' @param shocks Named list: variable -> numeric deviation path (length n).
#' @param hist Macro history (for derived variables).
#' @param name,narrative Scenario label and description.
#' @return Scenario data frame with attributes `name`, `narrative`, `shocks`.
#' @export
scenario_apply <- function(base, shocks, hist, name, narrative = "") {
  s <- base[, c("date", scenario_core_vars())]
  for (v in names(shocks)) s[[v]] <- s[[v]] + shocks[[v]][seq_len(nrow(s))]
  s$repo <- pmax(s$repo, 0.25)
  s <- scenario_derive(s, hist)
  attr(s, "name") <- name; attr(s, "narrative") <- narrative; attr(s, "shocks") <- shocks
  s
}

#' The default scenario library: baseline, upside and downside
#'
#' Three internally consistent South African scenarios over `n` quarters, the
#' upside and downside as deviations from the baseline (ramp, hold, decay).
#' Calibrations are judgemental and anchored on history and on the SARB's own
#' stress-test design:
#'
#' * **Upside**: faster disinflation and a growth dividend -- oil and food
#'   prices ease, the 3% target is met early and the SARB cuts a further
#'   100 bps; terms-of-trade gains and electricity and logistics reform lift
#'   growth; the rand firms and term premia compress (2009-10 and 2024-25
#'   easing, the 2010-11 commodity upswing).
#' * **Downside**: a stagflationary shock of the kind the SARB's 2023 common
#'   scenario stress test describes -- a fiscal slippage and a rand sell-off
#'   push imported inflation up, the SARB tightens a further 150 bps, the
#'   long bond sells off 200 bps and the economy contracts (2015-16 and
#'   2022-23 stress, with a recession on top).
#'
#' [scenario_downside_cuts()] gives the same real shock with the opposite
#' policy response (a demand-led recession in which the SARB cuts, as in 2009
#' and 2020), the key judgement in the downside.
#' @param base Baseline scenario.
#' @param hist Macro history.
#' @return Named list of scenario data frames: `baseline`, `upside`,
#'   `downside`.
#' @export
scenario_library <- function(base, hist) {
  n <- nrow(base)
  sp <- function(...) shock_path(n, ...)
  b <- base; attr(b, "name") <- "Baseline"
  attr(b, "narrative") <- paste(
    "Inflation peaks above 5% around the turn of the year and returns to the 3% target by late 2027;",
    "the repo rate holds at 7.25% and then eases gradually to 6.25%; growth settles at about 1.6%.")
  list(
    baseline = b,
    upside = scenario_apply(base, list(
      gdp_yy = sp(1.5, ramp = 4, hold = 6), zar_yy = sp(-10, ramp = 3, hold = 3),
      gov10y = sp(-1.00, ramp = 3, hold = 6), cpi_yy = sp(-1.0, ramp = 3, hold = 4),
      repo = sp(-1.00, ramp = 4, hold = 6, half_life = 8), hpi_yy = sp(4, ramp = 4, hold = 6, delay = 1),
      unemp = sp(-1.5, ramp = 6, hold = 6, delay = 2), alsi_yy = sp(12, ramp = 2, hold = 2)),
      hist, "Upside: faster disinflation and a growth dividend",
      paste("Oil and food prices ease and inflation returns to the 3% target early; the SARB cuts a",
            "further 100 bps; terms-of-trade gains and electricity and logistics reform lift growth to",
            "about 3%; the rand firms and the long bond rallies about 100 bps.")),
    downside = scenario_apply(base, downside_shocks(n, policy = "hike"), hist,
      "Downside: stagflationary shock",
      paste("A fiscal slippage and a rand sell-off push inflation above 6%; the SARB hikes a further",
            "150 bps and the long bond sells off about 200 bps; the economy contracts for two years,",
            "unemployment rises 2.5 points and house prices fall."))
  )
}

#' Shock paths of the downside scenario
#'
#' The real shock (output, employment, house and equity prices, the rand) is
#' common to both versions; `policy` sets how inflation, the policy rate and
#' the long bond respond.
#' @param n Horizon (quarters).
#' @param policy `"hike"` (stagflation: imported inflation and tightening) or
#'   `"cut"` (a demand-led recession: disinflation and easing).
#' @return Named list of deviation paths.
#' @export
downside_shocks <- function(n, policy = c("hike", "cut")) {
  policy <- match.arg(policy)
  sp <- function(...) shock_path(n, ...)
  real <- list(gdp_yy = sp(-3.0, ramp = 4, hold = 3, half_life = 4, delay = 1),
               unemp = sp(2.5, ramp = 6, hold = 4, half_life = 6, delay = 1),
               hpi_yy = sp(-6, ramp = 4, hold = 3, delay = 1),
               alsi_yy = sp(-15, ramp = 2, hold = 1))
  nominal <- if (policy == "hike")
    list(zar_yy = sp(20, ramp = 2, hold = 2), cpi_yy = sp(2.0, ramp = 4, hold = 3),
         repo = sp(1.50, ramp = 4, hold = 4, half_life = 6), gov10y = sp(2.00, ramp = 2, hold = 4, half_life = 6))
  else
    list(zar_yy = sp(8, ramp = 1, hold = 1), cpi_yy = sp(-1.0, ramp = 4, hold = 4, delay = 1),
         repo = sp(-1.50, ramp = 4, hold = 4, delay = 1, half_life = 6), gov10y = sp(0.50, ramp = 2, hold = 2))
  c(real, nominal)
}

#' The downside with the opposite policy response
#'
#' Same real shock as the downside, but the SARB cuts into a demand-led
#' recession (as in 2009 and 2020) instead of tightening against imported
#' inflation. Used to show how much of the downside's positioning rests on
#' the policy-rate judgement.
#' @param base Baseline scenario.
#' @param hist Macro history.
#' @return Scenario data frame.
#' @export
scenario_downside_cuts <- function(base, hist) {
  scenario_apply(base, downside_shocks(nrow(base), policy = "cut"), hist,
                 "Downside with cuts: demand-led recession",
                 paste("The same contraction as the downside, but inflation falls and the SARB cuts a further",
                       "150 bps instead of hiking."))
}
