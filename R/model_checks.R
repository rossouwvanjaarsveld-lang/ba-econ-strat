# =============================================================================
#  model_checks.R -- is the ARDL satellite the right tool, and can it be
#                    improved?
#
#  Tools to test refinements of the Stage 1 satellites on the same footing as
#  the satellites themselves: dynamic pseudo-out-of-sample forecasts from
#  expanding windows, conditional on the realised macro path.
#
#    * Clark-West (2007) tests of whether a satellite beats its AR(1)
#      benchmark out of sample by more than chance (the benchmark is nested
#      in the satellite, so a plain Diebold-Mariano test is biased against
#      the larger model);
#    * Pesaran-Shin-Smith (2001) bounds test for a level relationship (is a
#      deposit cost tied to the repo rate in the long run, or is the levels
#      regression spurious?), with small-sample critical values simulated
#      for the actual sample size;
#    * asymmetric pass-through in the spirit of Shin, Yu and Greenwood-Nimmo
#      (2014): the repo rate split into partial sums of increases and
#      decreases, so hikes and cuts may pass through differently;
#    * convex credit losses: a log transform of the credit-loss ratio and a
#      kink in GDP growth below zero;
#    * the intercept correction evaluated at every pseudo-out-of-sample
#      origin, not only at the two back-test origins.
# =============================================================================

#' Rebuild the estimation panel of a fitted satellite
#'
#' [fit_stage1()] adds impulse dummies to the panel before the search; their
#' names end in the flagged quarter (`YYYYMM`), so they can be rebuilt.
#' @param sat A `baeconstrat_satellite`.
#' @param panel The Stage 1 panel the satellite was fitted on.
#' @return `panel` restricted to the satellite's sample, with its control
#'   dummies added.
#' @export
satellite_panel <- function(sat, panel) {
  p <- panel
  if (!is.null(sat$sample_from)) p <- p[p$date >= as.Date(sat$sample_from), , drop = FALSE]
  for (cc in setdiff(sat$controls %||% character(0), names(p))) {
    m <- regmatches(cc, regexpr("[0-9]{6}$", cc))
    p[[cc]] <- if (grepl("^sm[0-9]{2}$", cc)) as.numeric(as.integer(format(p$date, "%m")) == as.integer(substr(cc, 3, 4)))
               else if (length(m)) as.numeric(format(p$date, "%Y%m") == m) else 0
  }
  p
}

#' Split a driver into positive and negative components
#'
#' For a level variable (`type = "partial_sum"`) the components are the
#' cumulative sums of its increases and of its decreases, so that
#' `x_t = x_0 + x_pos_t + x_neg_t` (Shin, Yu and Greenwood-Nimmo, 2014). For a
#' change variable (`type = "split"`) they are `max(x, 0)` and `min(x, 0)`.
#' @param d Data frame ordered by date.
#' @param var Column to decompose.
#' @param type `"partial_sum"` or `"split"`.
#' @return `d` with `<var>_pos` and `<var>_neg` added.
#' @export
asym_decompose <- function(d, var, type = c("partial_sum", "split")) {
  type <- match.arg(type)
  x <- d[[var]]
  if (type == "split") {
    d[[paste0(var, "_pos")]] <- pmax(x, 0)
    d[[paste0(var, "_neg")]] <- pmin(x, 0)
    return(d)
  }
  dx <- c(NA, diff(x))
  dx[!is.finite(dx)] <- 0
  pos <- cumsum(pmax(dx, 0)); neg <- cumsum(pmin(dx, 0))
  pos[!is.finite(x)] <- NA; neg[!is.finite(x)] <- NA
  d[[paste0(var, "_pos")]] <- pos
  d[[paste0(var, "_neg")]] <- neg
  d
}

#' Dynamic pseudo-out-of-sample forecasts of one specification
#'
#' Re-estimates the specification on an expanding window at each origin and
#' simulates it `h` quarters ahead along the realised drivers, exactly as
#' [satellite_search()] ranks specifications, but returns the forecasts so
#' that alternatives can be compared on common origins. Optional log
#' transform of the target (forecasts are returned on the original scale)
#' and optional intercept correction (mean residual of the last `ic_k`
#' unflagged training quarters, re-computed at every origin).
#' @param panel Estimation panel (see [satellite_panel()]).
#' @param target,drivers,lags,ar,h,min_train,controls As in [satellite_fit()].
#' @param transform `"none"` or `"log"`.
#' @param ic_k Intercept-correction window (0 = none).
#' @param ic_mode `"level"` shifts the simulated path by the mean residual;
#'   `"equation"` adds it to the intercept inside the simulation (see
#'   [satellite_project()]).
#' @param origin_dates Optional origin dates; default every expanding-window
#'   origin after `min_train` usable quarters.
#' @return Data frame `origin, date, h, actual, fc`.
#' @export
spec_oos <- function(panel, target, drivers, lags, ar = TRUE, h = 4L, min_train = 28L,
                     controls = character(0), transform = c("none", "log"), ic_k = 0L,
                     ic_mode = c("level", "equation"), origin_dates = NULL) {
  transform <- match.arg(transform); ic_mode <- match.arg(ic_mode)
  y_raw <- panel[[target]]
  pz <- panel
  if (transform == "log") pz[[target]] <- ifelse(y_raw > 0, log(y_raw), NA_real_)
  dz <- .sat_design(pz, target, drivers, lags, ar, controls)
  idx <- which(dz$ok); n <- length(dz$y)
  origins <- if (is.null(origin_dates)) idx[idx >= idx[min_train] & idx <= max(idx) - 1L]
             else which(panel$date %in% as.Date(origin_dates))
  ctl <- intersect(controls, colnames(dz$X))
  out <- vector("list", length(origins))
  for (i in seq_along(origins)) {
    o <- origins[i]
    tr <- idx[idx <= o]
    if (length(tr) < ncol(dz$X) + 2L) next
    b <- stats::lm.fit(dz$X[tr, , drop = FALSE], dz$y[tr])$coefficients
    b[is.na(b)] <- 0
    ic <- 0
    if (ic_k > 0L) {
      fl <- if (length(ctl)) rowSums(dz$X[tr, ctl, drop = FALSE] != 0) > 0 else rep(FALSE, length(tr))
      rr <- utils::tail(tr[!fl], ic_k)
      ic <- mean(dz$y[rr] - as.numeric(dz$X[rr, , drop = FALSE] %*% b))
    }
    if (ic_mode == "equation" && ic != 0) {
      b2 <- b; b2[["(Intercept)"]] <- b2[["(Intercept)"]] + ic
      p <- .sat_simulate(b2, dz$X, dz$y, o, h, ar)
    } else p <- .sat_simulate(b, dz$X, dz$y, o, h, ar) + ic
    tt <- o + seq_len(h)
    act <- rep(NA_real_, h); act[tt <= n] <- y_raw[tt[tt <= n]]
    dt <- rep(as.Date(NA), h); dt[tt <= n] <- panel$date[tt[tt <= n]]
    out[[i]] <- data.frame(origin = panel$date[o], date = dt, h = seq_len(h), actual = act,
                           fc = if (transform == "log") exp(p) else p)
  }
  out <- do.call(rbind, out)
  out[is.finite(out$actual) & is.finite(out$fc), , drop = FALSE]
}

#' Clark-West test of equal predictive accuracy for nested models
#'
#' Tests whether the larger model forecasts better than the nested benchmark,
#' correcting the MSPE difference for the noise the larger model adds by
#' estimating parameters that are zero under the null (Clark and West, 2007).
#' @param e_bench,e_model Forecast errors of the benchmark and the larger model
#'   on the same targets.
#' @param lag HAC truncation lag (use `h - 1` for `h`-step forecasts).
#' @return Named numeric: `stat` (t-statistic), `p` (one-sided), `mspe_ratio`.
#' @export
clark_west <- function(e_bench, e_model, lag = 0L) {
  ok <- is.finite(e_bench) & is.finite(e_model)
  eb <- e_bench[ok]; em <- e_model[ok]
  f <- eb^2 - em^2 + (em - eb)^2
  n <- length(f)
  if (n < 5L) return(c(stat = NA_real_, p = NA_real_, mspe_ratio = NA_real_))
  v <- hac_vcov(matrix(1, n, 1, dimnames = list(NULL, "m")), f - mean(f), lag = lag)[1, 1]
  st <- mean(f) / sqrt(v)
  c(stat = st, p = stats::pnorm(st, lower.tail = FALSE), mspe_ratio = mean(em^2) / mean(eb^2))
}

#' Diebold-Mariano test of equal squared-error loss
#'
#' For non-nested comparisons (e.g. the same model with and without an
#' intercept correction). Positive statistics favour model `b`.
#' @param e_a,e_b Forecast errors of the two models on the same targets.
#' @param lag HAC truncation lag.
#' @return Named numeric: `stat`, `p` (two-sided), `mspe_ratio` (b over a).
#' @export
dm_test <- function(e_a, e_b, lag = 0L) {
  ok <- is.finite(e_a) & is.finite(e_b)
  d <- e_a[ok]^2 - e_b[ok]^2
  n <- length(d)
  if (n < 5L) return(c(stat = NA_real_, p = NA_real_, mspe_ratio = NA_real_))
  v <- hac_vcov(matrix(1, n, 1, dimnames = list(NULL, "m")), d - mean(d), lag = lag)[1, 1]
  st <- mean(d) / sqrt(v)
  c(stat = st, p = 2 * stats::pnorm(-abs(st)), mspe_ratio = mean(e_b[ok]^2) / mean(e_a[ok]^2))
}

#' HAC Wald test that two coefficients are equal
#' @param fit Output of [ols_hac()].
#' @param a,b Coefficient names.
#' @return Named numeric: `diff`, `se`, `t`, `p` (two-sided).
#' @export
wald_equal <- function(fit, a, b) {
  g <- stats::setNames(rep(0, length(fit$coef)), names(fit$coef))
  g[a] <- 1; g[b] <- -1
  d <- sum(g * fit$coef)
  se <- sqrt(as.numeric(t(g) %*% fit$vcov %*% g))
  tt <- d / se
  c(diff = d, se = se, t = tt, p = 2 * stats::pnorm(-abs(tt)))
}

#' Pesaran-Shin-Smith bounds test for a level relationship
#'
#' Unrestricted error-correction regression
#' `dy_t = a + pi_y y_{t-1} + pi_x x_{t-1} + sum_j g_j dx_{t-j} + sum_j f_j dy_{t-j} + e_t`
#' (case III: unrestricted intercept, no trend) and the F-statistic for
#' `pi_y = pi_x = 0`. Critical bounds are simulated for the actual sample size:
#' the lower bound with a stationary regressor, the upper with a random walk,
#' both under the null that `y` is a random walk unrelated to `x`.
#' @param y Numeric series.
#' @param x Numeric vector or matrix of level regressors (same length as `y`);
#'   with the repo rate split into partial sums of increases and decreases
#'   this is the asymmetric ("hidden cointegration") version of the test.
#' @param p Lags of the differences.
#' @param controls Optional matrix of extra regressors (e.g. impulse dummies).
#' @param nsim Monte Carlo replications for the bounds.
#' @param seed Seed for the simulation.
#' @return List: `F`, `n`, `k`, `bounds` (matrix, rows 90/95/99%, columns
#'   I0/I1), `decision` at 5%, `speed` (pi_y) and `long_run` (-pi_x / pi_y,
#'   one per regressor).
#' @export
bounds_test <- function(y, x, p = 1L, controls = NULL, nsim = 5000L, seed = 1L) {
  x <- as.matrix(x); k <- ncol(x)
  build <- function(y, x, ctl) {
    dy <- c(NA, diff(y)); dx <- rbind(NA, diff(x))
    X <- cbind(1, ylag = lag_vec(y, 1L), apply(x, 2, lag_vec, k = 1L), dx)
    for (j in seq_len(p)) X <- cbind(X, apply(dx, 2, lag_vec, k = j), lag_vec(dy, j))
    if (!is.null(ctl)) X <- cbind(X, ctl)
    ok <- stats::complete.cases(cbind(dy, X))
    list(y = dy[ok], X = X[ok, , drop = FALSE])
  }
  lev <- 2:(2 + k)
  fstat <- function(z) {
    u <- stats::lm.fit(z$X, z$y)$residuals
    r <- stats::lm.fit(z$X[, -lev, drop = FALSE], z$y)$residuals
    q <- length(lev); kk <- ncol(z$X); n <- length(z$y)
    ((sum(r^2) - sum(u^2)) / q) / (sum(u^2) / (n - kk))
  }
  z <- build(y, x, controls)
  f <- fstat(z)
  cf <- stats::lm.fit(z$X, z$y)$coefficients
  n <- length(y)
  set.seed(seed)
  sim <- matrix(NA_real_, nsim, 2, dimnames = list(NULL, c("I0", "I1")))
  for (s in seq_len(nsim)) {
    yy <- cumsum(stats::rnorm(n))
    e <- matrix(stats::rnorm(n * k), n, k)
    sim[s, "I0"] <- fstat(build(yy, e, NULL))
    sim[s, "I1"] <- fstat(build(yy, apply(e, 2, cumsum), NULL))
  }
  bnd <- apply(sim, 2, stats::quantile, probs = c(0.90, 0.95, 0.99))
  dec <- if (f > bnd["95%", "I1"]) "level relationship" else if (f < bnd["95%", "I0"]) "no level relationship"
         else "inconclusive"
  list(F = f, n = length(z$y), k = k, bounds = bnd, decision = dec,
       speed = unname(cf[2]), long_run = unname(-cf[3:(2 + k)] / cf[2]))
}

#' Compare two specifications of one target out of sample
#'
#' Both specifications are re-estimated at every common origin and simulated
#' along the realised drivers; the second must nest the first for the
#' Clark-West test to apply.
#' @param panel Estimation panel.
#' @param target Target name.
#' @param a,b Specifications: lists with `drivers`, `lags`, and optionally
#'   `transform` and `ic_k`.
#' @param h,min_train,controls As in [spec_oos()].
#' @return List: `rmse_a`, `rmse_b`, `ratio` (b over a), `cw1` and `cwh`
#'   (Clark-West at horizon 1 and `h`, b against a; meaningful when b nests
#'   a), `dm` (Diebold-Mariano on all horizons pooled, HAC lag `h`),
#'   `n_origins`, `paths`.
#' @export
spec_compare <- function(panel, target, a, b, h = 4L, min_train = 28L, controls = character(0)) {
  run <- function(s) spec_oos(panel, target, s$drivers, s$lags, h = h, min_train = min_train,
                              controls = controls, transform = s$transform %||% "none",
                              ic_k = s$ic_k %||% 0L, ic_mode = s$ic_mode %||% "level")
  fa <- run(a); fb <- run(b)
  m <- merge(fa, fb, by = c("origin", "h"), suffixes = c("_a", "_b"))
  m$ea <- m$actual_a - m$fc_a; m$eb <- m$actual_b - m$fc_b
  m <- m[order(m$origin, m$h), ]
  cw <- function(hh) { z <- m[m$h == hh, ]; clark_west(z$ea, z$eb, lag = hh - 1L) }
  list(rmse_a = sqrt(mean(m$ea^2)), rmse_b = sqrt(mean(m$eb^2)),
       ratio = sqrt(mean(m$eb^2) / mean(m$ea^2)), cw1 = cw(1L), cwh = cw(h),
       dm = dm_test(m$ea, m$eb, lag = h), n_origins = length(unique(m$origin)), paths = m)
}

#' Pass-through of a permanent policy-rate move to a satellite target
#'
#' Projects the satellite (model average, no intercept correction) along two
#' paths that differ only by a permanent repo move of `step` percentage points
#' from the quarter after the target's last observation: the repo level, its
#' quarterly change and the partial sums of increases and decreases move
#' accordingly; every other driver is held at its last value (so the curve
#' shifts in parallel). The difference is the pass-through, by quarter.
#' @param sat A `baeconstrat_satellite`.
#' @param history Quarterly history with the target and drivers.
#' @param step Repo move (percentage points); negative for a cut.
#' @param n Quarters to project.
#' @return Data frame `quarter, response` (target units per `step`).
#' @export
satellite_pass_through <- function(sat, history, step = 1, n = 40L) {
  h <- history[order(history$date), , drop = FALSE]
  last_y <- max(h$date[is.finite(h[[sat$target]])])
  h <- h[h$date <= last_y, , drop = FALSE]
  drv <- unique(c(sat$spec$d, unlist(lapply(sat$bma$specs, `[[`, "d"))))
  h <- locf_tail(h, drv)
  hl <- utils::tail(h, 1)
  base <- data.frame(date = next_quarters(last_y, n))
  for (v in drv) base[[v]] <- if (v == "d_repo") 0 else hl[[v]]
  shk <- base
  if ("repo" %in% drv) shk$repo <- base$repo + step
  if ("d_repo" %in% drv) shk$d_repo[1] <- step
  if ("repo_pos" %in% drv) shk$repo_pos <- base$repo_pos + max(step, 0)
  if ("repo_neg" %in% drv) shk$repo_neg <- base$repo_neg + min(step, 0)
  if ("real_repo" %in% drv) shk$real_repo <- base$real_repo + step
  p0 <- satellite_project(sat, h, base, method = "bma")
  p1 <- satellite_project(sat, h, shk, method = "bma")
  data.frame(quarter = seq_len(n), response = (p1$value - p0$value) / step)
}
