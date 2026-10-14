# =============================================================================
#  multipliers.R -- dynamic multipliers of the Stage 1 satellites
#
#  A dynamic multiplier is the path of a target after a macro shock, all else
#  equal. For an ARDL with one driver at lag l,
#
#     y_t = c + rho y_{t-1} + beta x_{t-l} + e_t,
#
#  a permanent unit change in x moves y by 0 until period l, then by
#  beta (1 + rho + ... + rho^(h-l)), converging to the long-run multiplier
#  beta / (1 - rho). The satellites are model averages whose drivers include
#  transformations of the same variable (the repo level, its one-period and
#  four-period changes, the real repo rate, partial sums of hikes and cuts),
#  so the multipliers here are computed by simulation: the model is
#  projected along a flat macro path and along the same path with the shock,
#  every derived driver recomputed by derive_drivers(), and the difference
#  is the multiplier. That is exactly how the satellites are used in a
#  scenario. Parameter uncertainty is shown by redrawing the coefficients of
#  the averaged models from their HAC covariance.
# =============================================================================

#' Core macro variables held fixed (or shocked) in a multiplier experiment
#' @return Character vector.
#' @export
multiplier_core_vars <- function() {
  c("repo", "gov10y", "cpi_yy", "gdp_yy", "unemp", "hpi_yy", "zar_yy", "alsi_yy", "coinc_yy",
    "lead_yy")
}

#' Flat and shocked macro paths for a multiplier experiment
#'
#' @param hist Macro history (quarterly or monthly panel) holding the core
#'   variables and their derived drivers.
#' @param n Horizon (periods).
#' @param shock Named list: core variable -> deviation (a scalar for a
#'   permanent shift, or a vector of length `n`).
#' @param freq `"quarter"` or `"month"`.
#' @return List `base`, `shocked` (data frames with `date` and every driver).
#' @export
multiplier_paths <- function(hist, n, shock, freq = c("quarter", "month")) {
  freq <- match.arg(freq)
  hist <- hist[order(hist$date), , drop = FALSE]
  core <- intersect(multiplier_core_vars(), names(hist))
  hist <- locf_tail(hist, core)
  last <- max(hist$date[is.finite(hist$repo)])
  hist <- hist[hist$date <= last, , drop = FALSE]
  step <- if (freq == "month") "month" else "3 months"
  dates <- seq(last, by = step, length.out = n + 1L)[-1]
  if (freq == "quarter") dates <- quarter_end(dates)
  k <- if (freq == "month") 13L else 5L
  tail_h <- utils::tail(hist, k)
  jib <- utils::tail(hist$jibar3m - hist$repo, 1)
  if (!is.finite(jib)) jib <- 0.35
  mk <- function(sh) {
    p <- data.frame(date = dates)
    for (v in core) {
      x <- rep(utils::tail(hist[[v]], 1), n)
      if (!is.null(sh[[v]])) x <- x + rep_len(sh[[v]], n)
      p[[v]] <- x
    }
    p$prime <- NA_real_; p$jibar3m <- NA_real_
    z <- rbind_fill(list(tail_h[, intersect(c("date", core, "prime", "jibar3m"), names(tail_h))], p))
    z$jibar3m[is.na(z$jibar3m)] <- z$repo[is.na(z$jibar3m)] + jib
    st <- c(tail_h$repo_pos[1] %||% 0, tail_h$repo_neg[1] %||% 0)
    z <- derive_drivers(z, freq, jib_spread = jib, repo_start = st)
    z[z$date %in% dates, , drop = FALSE]
  }
  list(base = mk(list()), shocked = mk(shock))
}

#' Dynamic multiplier of a satellite by simulation
#'
#' @param sat A `baeconstrat_satellite`.
#' @param history Panel the satellite is estimated on (targets and drivers).
#' @param shock Named list of core-variable deviations (see
#'   [multiplier_paths()]).
#' @param n Horizon (periods).
#' @param freq `"quarter"` or `"month"`.
#' @param cumulate For growth-rate targets: return the cumulative effect on
#'   the level, in percent (the annualised growth deviations summed and
#'   divided by 4 or 12).
#' @param n_draws Coefficient draws for the uncertainty band (0 = none).
#' @param probs Quantiles of the band.
#' @param seed RNG seed for the draws.
#' @return Data frame `h` (periods after the shock), `effect`, and `lo`, `hi`
#'   when `n_draws > 0`.
#' @export
satellite_dynamic_multiplier <- function(sat, history, shock, n = 12L, freq = c("quarter", "month"),
                                         cumulate = FALSE, n_draws = 0L, probs = c(0.05, 0.95),
                                         seed = 1L) {
  freq <- match.arg(freq)
  ## drivers published with a lag (GDP, unemployment, house prices) are
  ## carried forward to the jump-off, as in project_satellites()
  drv <- unique(c(sat$spec$d, unlist(lapply(sat$bma$specs, `[[`, "d"))))
  history <- locf_tail(history[order(history$date), , drop = FALSE], intersect(drv, names(history)))
  pp <- multiplier_paths(history, n, shock, freq)
  ## the projection starts after the last observation of the target; macro
  ## periods between it and the path's start keep their actual values
  last_y <- max(history$date[is.finite(history[[sat$target]]) & history$date < min(pp$base$date)])
  h <- history[history$date <= last_y, , drop = FALSE]
  gap <- history[history$date > last_y & history$date < min(pp$base$date), , drop = FALSE]
  bridge <- function(p) if (nrow(gap)) rbind_fill(list(gap[, intersect(names(p), names(gap))], p)) else p
  scale <- if (freq == "month") 12 else 4
  one <- function(s) {
    b <- satellite_project(s, h, bridge(pp$base), method = "bma")$value
    x <- satellite_project(s, h, bridge(pp$shocked), method = "bma")$value
    d <- utils::tail(x - b, n)
    if (cumulate) d <- cumsum(d) / scale
    d
  }
  out <- data.frame(h = seq_len(n), effect = one(sat))
  if (n_draws > 0L && !is.null(sat$bma) && !is.null(sat$bma$vcovs)) {
    set.seed(seed)
    w <- sat$bma$weights
    draws <- matrix(NA_real_, n_draws, n)
    for (r in seq_len(n_draws)) {
      i <- if (length(w) > 1L) sample.int(length(w), 1L, prob = w) else 1L
      s <- sat
      s$bma <- NULL; s$comb <- NULL
      s$spec <- sat$bma$specs[[i]]
      cf <- sat$bma$coefs[[i]]; V <- sat$bma$vcovs[[i]]
      L <- tryCatch(chol(V + diag(1e-12, nrow(V))), error = function(e) NULL)
      if (is.null(L)) next
      s$fit$coef <- cf + drop(t(L) %*% stats::rnorm(length(cf)))
      if (isTRUE(s$ar) && abs(s$fit$coef[["ar1"]]) >= 0.995) next
      draws[r, ] <- one(s)
    }
    q <- apply(draws, 2, stats::quantile, probs = probs, na.rm = TRUE)
    out$lo <- q[1, ]; out$hi <- q[2, ]
  }
  out
}

#' Local-projection impulse response (Jorda, 2005) with HAC bands
#'
#' Regresses `y_{t+h} - y_{t-1}` on the impulse variable's change at `t` and
#' controls (lags of the change in `y` and of the impulse, and current
#' controls), horizon by horizon. A model-free check on the ARDL dynamics.
#' @param d Data frame ordered by date.
#' @param y Response column (level or rate).
#' @param x Impulse column (level; its first difference is the impulse).
#' @param controls Additional control columns (entered at `t`).
#' @param H Maximum horizon.
#' @param p Lags of the differenced response and impulse used as controls.
#' @param level Coverage of the band.
#' @return Data frame `h`, `beta`, `se`, `lo`, `hi`, `n`.
#' @export
local_projection <- function(d, y, x, controls = character(0), H = 12L, p = 2L, level = 0.90) {
  d <- d[order(d$date), , drop = FALSE]
  yy <- d[[y]]; xx <- d[[x]]
  dx <- c(NA, diff(xx)); dy <- c(NA, diff(yy))
  z <- stats::qnorm(1 - (1 - level) / 2)
  out <- list()
  for (h in 0:H) {
    lead <- if (h == 0L) yy else c(yy[-seq_len(h)], rep(NA, h))
    resp <- lead - lag_vec(yy, 1L)
    X <- cbind(`(Intercept)` = 1, impulse = dx)
    for (k in seq_len(p)) X <- cbind(X, lag_vec(dy, k), lag_vec(dx, k))
    for (cc in controls) X <- cbind(X, d[[cc]])
    colnames(X) <- make.unique(c("(Intercept)", "impulse", rep("c", ncol(X) - 2L)))
    ok <- stats::complete.cases(resp, X)
    if (sum(ok) < ncol(X) + 10L) next
    f <- ols_hac(resp[ok], X[ok, , drop = FALSE], hac_lag = max(h + 1L, nw_bandwidth(sum(ok))))
    out[[length(out) + 1L]] <- data.frame(h = h, beta = f$coef[["impulse"]], se = f$se[["impulse"]],
                                          n = f$n)
  }
  r <- do.call(rbind, out)
  r$lo <- r$beta - z * r$se; r$hi <- r$beta + z * r$se
  r
}
