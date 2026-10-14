# =============================================================================
#  performance.R -- macro-adjusted performance measurement
#
#  A product's growth is the market's growth plus the bank's share effect.
#  The first part is the macro environment (Stage 1): no bank controls it.
#  The second is what management controls, and Stage 2 says what a typical
#  large bank achieves in the same macro regimes. So performance can be
#  judged against two macro-adjusted benchmarks:
#
#    par       the share effect of the typical large bank (population mean),
#    frontier  the share effect of a top-quintile large bank (80th percentile
#              of the population's posterior predictive),
#
#  and NGFB's percentile in that distribution says how much better it could
#  have done. For a scenario, the share effects are summed over the
#  scenario's sequence of regimes and market growth, quarter by quarter.
# =============================================================================

#' Share-effect totals of a parameter vector over a sequence of quarters
#' @param par Named parameters (`contraction`, `hiking`, `easing`, `kappa`).
#' @param regimes Regime labels, one per quarter.
#' @param g Sector growth per quarter (annualised \%).
#' @param mean_g Centring growth.
#' @return Sum over quarters of `400 dlog s` (drift excluded).
#' @export
share_effect_total <- function(par, regimes, g, mean_g) {
  sum(share_growth(par, regimes, g, mean_g, drift = FALSE))
}

#' Scenario scorecard: NGFB's share performance against macro-adjusted par
#'
#' @param ngfb_par NGFB's share parameters (product x param).
#' @param hyper Population hyperparameters.
#' @param regimes Scenario regimes (one per quarter).
#' @param proj Stage 1 projection (`date`, `g_<product>`), scenario quarters.
#' @param mean_g Named centring growth per product.
#' @param products Products.
#' @param frontier_q Frontier quantile.
#' @return Data frame by product: market growth, NGFB growth, NGFB share
#'   effect, par, frontier (all average percentage points a year), NGFB's
#'   percentile, the cumulative change in NGFB's market share over the
#'   scenario (percent of the starting share) and the verdict (below the
#'   30th percentile "well below par", 30th-50th "below par", 50th-70th
#'   "above par", from the 70th "near the frontier").
#' @export
scenario_scorecard <- function(ngfb_par, hyper, regimes, proj, mean_g, products, frontier_q = 0.80) {
  hyper$sd <- sqrt(hyper$tau^2 + hyper$se_mu^2)
  hp <- function(p, k, v) { x <- hyper[hyper$product == p & hyper$param == k, v]; if (length(x)) x[1] else 0 }
  n <- length(regimes)
  out <- list()
  for (p in intersect(products, rownames(ngfb_par))) {
    g <- proj[[paste0("g_", p)]][seq_len(n)]
    k <- sum(g - mean_g[[p]])
    cnt <- vapply(c("contraction", "hiking", "easing"), function(r) sum(regimes == r), 0)
    s_n <- share_effect_total(ngfb_par[p, ], regimes, g, mean_g[[p]])
    s_p <- sum(cnt * vapply(names(cnt), function(r) hp(p, r, "mu"), 0)) + k * hp(p, "kappa", "mu")
    sd <- sqrt(sum((cnt * vapply(names(cnt), function(r) hp(p, r, "sd"), 0))^2) + (k * hp(p, "kappa", "sd"))^2)
    pct <- if (sd > 0) stats::pnorm((s_n - s_p) / sd) else NA_real_
    out[[length(out) + 1L]] <- data.frame(
      product = p, market_growth = mean(g), ngfb_growth = mean(g) + s_n / n,
      share_effect = s_n / n, par = s_p / n, frontier = (s_p + stats::qnorm(frontier_q) * sd) / n,
      percentile = pct, share_change_pct = 100 * (exp(s_n / 400) - 1),
      stringsAsFactors = FALSE)
  }
  r <- do.call(rbind, out)
  r$verdict <- cut(r$percentile, c(-Inf, 0.3, 0.5, 0.7, Inf), right = FALSE,
                   labels = c("Well below par", "Below par", "Above par", "Near the frontier"))
  r$verdict <- as.character(r$verdict)
  r$gap_to_frontier <- r$frontier - r$share_effect
  r
}

#' Macro-implied ("par") growth of a sector target over rolling windows
#'
#' From each quarter, the satellite is simulated `window` quarters ahead
#' along the realised macro path, starting from the actual value: what the
#' macro environment alone implied for the next year. Actual minus implied
#' is the industry's performance against macro par.
#' @param sat A `baeconstrat_satellite` (volume target, annualised growth).
#' @param panel Its estimation panel ([satellite_panel()]).
#' @param window Quarters.
#' @param method `"bma"` or `"best"`.
#' @return Data frame `date` (window end), `actual`, `implied` (average
#'   annualised growth over the window).
#' @export
macro_implied_growth <- function(sat, panel, window = 4L, method = "bma") {
  y <- panel[[sat$target]]
  ok <- which(is.finite(y))
  out <- list()
  for (o in ok) {
    tt <- o + seq_len(window)
    if (max(tt) > nrow(panel) || any(!is.finite(y[tt]))) next
    h <- panel[seq_len(o), , drop = FALSE]
    p <- panel[tt, , drop = FALSE]
    v <- tryCatch(satellite_project(sat, h, p, method = method)$value, error = function(e) rep(NA_real_, window))
    if (any(!is.finite(v))) next
    out[[length(out) + 1L]] <- data.frame(date = panel$date[max(tt)], actual = mean(y[tt]), implied = mean(v))
  }
  do.call(rbind, out)
}
