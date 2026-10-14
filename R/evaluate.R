# =============================================================================
#  evaluate.R -- one out-of-sample harness for every model class
#
#  Each model is reduced to a list of "designs" (a response vector, a design
#  matrix aligned to the panel's rows, and the rows usable for estimation).
#  From every forecast origin the designs are re-estimated on data up to the
#  origin, simulated forward conditional on the realised macro path (as in a
#  scenario), and averaged with BIC weights recomputed on the training
#  window. Monthly forecasts are averaged into quarters, so monthly and
#  quarterly models are scored against the same quarterly outcomes.
#
#  Model classes:
#    * the sign-restricted ARDL satellites (single lag per driver), averaged;
#    * polynomial distributed lags (Almon): each driver enters through a
#      quadratic lag polynomial over 0..L periods, so a monthly model can
#      spread a rate change over a year with three parameters instead of
#      thirteen; the sign restriction applies to the sum of the lag weights;
#    * Bayesian shrinkage ("BVAR-X equation"): every candidate driver at every
#      lag, coefficients shrunk towards zero with a prior variance that
#      decays with the lag (Minnesota-style), the tightness chosen by the
#      marginal likelihood -- shrinkage instead of selection, no signs;
#    * the AR(1) benchmark.
# =============================================================================

#' Design of one ARDL specification on a panel
#' @param panel Panel (rows aligned with the output).
#' @param target Target column.
#' @param spec List `d` (drivers), `l` (lags).
#' @param ar Include the lagged target.
#' @param controls Control columns.
#' @return List `y`, `X`, `ok`.
#' @export
ardl_design <- function(panel, target, spec, ar = TRUE, controls = character(0)) {
  .sat_design(panel, target, spec$d, spec$l, ar, controls)
}

#' Design of a polynomial-distributed-lag (Almon) ARDL specification
#'
#' Driver `x` enters through `z_k = sum_{l=0}^{L} (l / L)^k x_{t-l}`,
#' `k = 0..degree`; the implied lag weights are `beta_l = sum_k theta_k (l/L)^k`.
#' @param panel Panel.
#' @param target Target column.
#' @param drivers Driver names.
#' @param L Maximum lag (periods).
#' @param degree Polynomial degree.
#' @param ar Include the lagged target.
#' @param controls Control columns.
#' @return List `y`, `X`, `ok`, `pdl` (driver, L, degree, columns).
#' @export
pdl_design <- function(panel, target, drivers, L = 12L, degree = 2L, ar = TRUE,
                       controls = character(0)) {
  y <- panel[[target]]
  cols <- list(`(Intercept)` = rep(1, length(y)))
  if (ar) cols[["ar1"]] <- lag_vec(y, 1L)
  for (cc in controls) cols[[cc]] <- panel[[cc]]
  info <- list()
  for (dv in drivers) {
    x <- panel[[dv]]
    lagm <- sapply(0:L, function(l) lag_vec(x, l))
    for (k in 0:degree) {
      nm <- sprintf("%s_P%d", dv, k)
      cols[[nm]] <- drop(lagm %*% ((0:L) / L)^k)
    }
    info[[dv]] <- list(driver = dv, L = L, degree = degree, cols = sprintf("%s_P%d", dv, 0:degree))
  }
  X <- do.call(cbind, cols)
  ok <- stats::complete.cases(cbind(y, X))
  list(y = y, X = X, ok = ok, pdl = info)
}

#' Sum of PDL lag weights and its HAC standard error
#' @param coef,vcov Coefficients and covariance from [ols_hac()].
#' @param info One element of the `pdl` list of [pdl_design()].
#' @return Named numeric `sum`, `se`.
#' @export
pdl_sum <- function(coef, vcov, info) {
  a <- vapply(0:info$degree, function(k) sum(((0:info$L) / info$L)^k), 0)
  b <- coef[info$cols]; V <- vcov[info$cols, info$cols, drop = FALSE]
  c(sum = sum(a * b), se = sqrt(max(as.numeric(t(a) %*% V %*% a), 0)))
}

#' Lag weights implied by a PDL specification
#' @param coef Named coefficients from [ols_hac()].
#' @param info One element of the `pdl` list of [pdl_design()].
#' @return Numeric vector of weights for lags `0..L`.
#' @export
pdl_weights <- function(coef, info) {
  P <- sapply(0:info$degree, function(k) ((0:info$L) / info$L)^k)
  drop(P %*% coef[info$cols])
}

#' Sign-restricted search over PDL specifications
#'
#' Subsets of at most `max_k` candidate drivers (one per economic group),
#' each entering through a quadratic Almon polynomial over `0..L`; a
#' specification is kept only if the SUM of each driver's lag weights has
#' the a-priori sign with |HAC t| >= `t_min`. BIC weights average the
#' survivors.
#' @inheritParams satellite_search
#' @param L Maximum lag of the polynomial.
#' @param degree Polynomial degree.
#' @return List: `target`, `designs` (survivors' design specifications),
#'   `table`, `weights`.
#' @export
pdl_search <- function(panel, target, candidates, signs, groups = NULL, max_k = 3L, L = 12L,
                       degree = 2L, ar = TRUE, t_min = 1.0, rho_max = 0.98, sample_from = NULL,
                       controls = character(0), required = character(0)) {
  if (!is.null(sample_from)) panel <- panel[panel$date >= as.Date(sample_from), , drop = FALSE]
  candidates <- intersect(candidates, names(panel))
  if (is.null(groups)) groups <- stats::setNames(candidates, candidates)
  grp <- groups[candidates]; grp[is.na(grp)] <- candidates[is.na(grp)]
  req <- intersect(required, candidates); free <- setdiff(candidates, req)
  subsets <- if (length(req)) list(req) else list()
  kmax <- min(max_k - length(req), length(free))
  for (k in seq_len(max(kmax, 0L))) {
    cmb <- utils::combn(free, k, simplify = FALSE)
    cmb <- Filter(function(cc) !anyDuplicated(grp[c(req, cc)]), cmb)
    subsets <- c(subsets, lapply(cmb, function(cc) c(req, cc)))
  }
  rows <- list(); keep <- list()
  for (i in seq_along(subsets)) {
    dv <- subsets[[i]]
    dz <- pdl_design(panel, target, dv, L, degree, ar, controls)
    if (sum(dz$ok) < 3L * (length(dv) * (degree + 1L) + 3L)) next
    f <- ols_hac(dz$y[dz$ok], dz$X[dz$ok, , drop = FALSE])
    rho <- if (ar) f$coef[["ar1"]] else 0
    if (abs(rho) >= rho_max) next
    sums <- t(vapply(dz$pdl, function(inf) pdl_sum(f$coef, f$vcov, inf), c(sum = 0, se = 0)))
    sg <- signs[dv]; sg[is.na(sg)] <- 0
    tt <- sums[, "sum"] / sums[, "se"]
    if (!all(sg == 0 | sign(sums[, "sum"]) == sg) || !all(abs(tt) >= t_min, na.rm = FALSE)) next
    keep[[length(keep) + 1L]] <- list(d = dv, L = L, degree = degree)
    rows[[length(rows) + 1L]] <- data.frame(spec = paste(sprintf("%s{0..%d}", dv, L), collapse = " + "),
                                            k = length(dv), bic = f$bic, rho = rho, min_abs_t = min(abs(tt)),
                                            stringsAsFactors = FALSE)
  }
  tab <- rbind_fill(rows)
  w <- if (is.null(tab)) numeric(0) else { v <- exp(-0.5 * (tab$bic - min(tab$bic))); v / sum(v) }
  list(target = target, ar = ar, controls = controls, specs = keep, table = tab, weights = w,
       sample_from = sample_from, L = L, degree = degree)
}

## ---- the out-of-sample engine ---------------------------------------------

# OLS on training rows -> coefficients and BIC
.fit_rows <- function(dz, rows) {
  X <- dz$X[rows, , drop = FALSE]; y <- dz$y[rows]
  keep <- colSums(abs(X)) > 0
  f <- stats::lm.fit(X[, keep, drop = FALSE], y)
  b <- stats::setNames(rep(0, ncol(X)), colnames(X)); b[keep] <- f$coefficients
  b[is.na(b)] <- 0
  e <- f$residuals; n <- length(y); k <- sum(keep)
  list(b = b, bic = n * log(sum(e^2) / n) + k * log(n))
}

#' Model-averaged dynamic forecasts from expanding-window origins
#'
#' @param designs List of designs (see [ardl_design()], [pdl_design()]), all
#'   aligned with the same panel rows.
#' @param origins Row indices of the forecast origins (last estimation row).
#' @param h Horizon (periods).
#' @param ar Designs include the lagged target as `ar1`.
#' @param weights `"bic"` (recomputed on each training window), `"equal"`,
#'   or a numeric vector of fixed weights.
#' @param min_train Minimum training observations.
#' @return Matrix (origins x h) of forecasts.
#' @export
oos_forecasts <- function(designs, origins, h, ar = TRUE, weights = "bic", min_train = 28L) {
  Fm <- matrix(NA_real_, length(origins), h)
  for (i in seq_along(origins)) {
    o <- origins[i]
    pr <- list(); bic <- numeric(0); ww <- numeric(0)
    for (j in seq_along(designs)) {
      dz <- designs[[j]]
      rows <- which(dz$ok & seq_along(dz$y) <= o)
      if (length(rows) < min_train) next
      f <- .fit_rows(dz, rows)
      p <- .sat_simulate(f$b, dz$X, dz$y, o, h, ar)
      if (all(!is.finite(p))) next
      pr[[length(pr) + 1L]] <- p; bic <- c(bic, f$bic)
      ww <- c(ww, if (is.numeric(weights)) weights[j] else 1)
    }
    if (!length(pr)) next
    w <- if (identical(weights, "bic")) exp(-0.5 * (bic - min(bic))) else ww
    w <- w / sum(w)
    M <- do.call(rbind, pr)
    Fm[i, ] <- colSums(M * w, na.rm = FALSE)
  }
  Fm
}

#' Designs of a satellite's model average (its largest-weight members)
#' @param sat A `baeconstrat_satellite`.
#' @param panel Panel ([satellite_panel()] adds the control dummies).
#' @param top Maximum number of members.
#' @return List of designs.
#' @export
satellite_designs <- function(sat, panel, top = 20L) {
  sp <- if (!is.null(sat$bma)) sat$bma$specs[order(-sat$bma$weights)] else list(sat$spec)
  sp <- utils::head(sp, top)
  lapply(sp, function(s) ardl_design(panel, sat$target, s, sat$ar, sat$controls %||% character(0)))
}

#' Bayesian shrinkage ARDL (Minnesota-style prior), fitted by marginal likelihood
#'
#' `y = X b + e`; unpenalised columns (intercept, AR term, controls) have a
#' flat prior, every driver-lag coefficient `b_j ~ N(0, s2 / (lambda w_j))`
#' with `w_j = (1 + lag_j)^2 * var(x_j) / var(y)`: longer lags are shrunk
#' harder, as in a Minnesota prior. `lambda` maximises the marginal
#' likelihood.
#' @param y Response.
#' @param X Design (columns named `<driver>_L<lag>` for penalised ones).
#' @param free Logical: unpenalised columns.
#' @param lambda Optional fixed tightness.
#' @return List `b`, `lambda`, `logml`.
#' @export
shrinkage_fit <- function(y, X, free, lambda = NULL) {
  n <- length(y)
  lag <- suppressWarnings(as.integer(sub("^.*_L([0-9]+)$", "\\1", colnames(X))))
  lag[is.na(lag)] <- 0L
  sx <- apply(X, 2, stats::sd); sy <- stats::sd(y)
  w <- ifelse(free, 0, (1 + lag)^2 * pmax(sx, 1e-8)^2 / sy^2)
  fit_l <- function(lam) {
    D <- diag(lam * w, ncol(X))
    A <- crossprod(X) + D
    R <- tryCatch(chol(A), error = function(e) NULL)
    if (is.null(R)) return(list(logml = -Inf))
    b <- backsolve(R, forwardsolve(t(R), crossprod(X, y)))
    rss <- sum((y - X %*% b)^2) + sum(lam * w * b^2)
    pen <- which(!free)
    ## log marginal likelihood (sigma^2 profiled; flat-prior columns cancel)
    logml <- -0.5 * (n - sum(free)) * log(rss) - sum(log(diag(R))) + 0.5 * sum(log(lam * w[pen]))
    list(b = drop(b), logml = logml)
  }
  if (is.null(lambda)) {
    o <- stats::optimize(function(l) fit_l(exp(l))$logml, c(log(1e-3), log(1e4)), maximum = TRUE)
    lambda <- exp(o$maximum)
  }
  f <- fit_l(lambda)
  list(b = stats::setNames(f$b, colnames(X)), lambda = lambda, logml = f$logml)
}

#' Out-of-sample forecasts of a Bayesian shrinkage ARDL
#' @param panel Panel.
#' @param target Target column.
#' @param candidates Candidate drivers (all enter, at every lag in `lags`).
#' @param lags Lags.
#' @param origins,h See [oos_forecasts()].
#' @param ar Include the lagged target.
#' @param controls Unpenalised controls.
#' @param min_train Minimum training observations.
#' @return Matrix (origins x h) of forecasts, attribute `lambda`.
#' @export
shrinkage_oos <- function(panel, target, candidates, lags, origins, h, ar = TRUE,
                          controls = character(0), min_train = 28L) {
  candidates <- intersect(candidates, names(panel))
  dz <- .sat_design(panel, target, rep(candidates, each = length(lags)),
                    rep(lags, times = length(candidates)), ar, controls)
  free <- colnames(dz$X) %in% c("(Intercept)", "ar1", controls)
  Fm <- matrix(NA_real_, length(origins), h); lam <- rep(NA_real_, length(origins))
  for (i in seq_along(origins)) {
    o <- origins[i]
    rows <- which(dz$ok & seq_along(dz$y) <= o)
    if (length(rows) < min_train) next
    X <- dz$X[rows, , drop = FALSE]
    keep <- colSums(abs(X)) > 0
    f <- shrinkage_fit(dz$y[rows], X[, keep, drop = FALSE], free[keep])
    b <- stats::setNames(rep(0, ncol(dz$X)), colnames(dz$X)); b[keep] <- f$b
    Fm[i, ] <- .sat_simulate(b, dz$X, dz$y, o, h, ar)
    lam[i] <- f$lambda
  }
  attr(Fm, "lambda") <- lam
  Fm
}

#' Average monthly forecasts into quarters
#'
#' Volumes (annualised monthly growth) and accounting-flow ratios average
#' exactly into the quarterly target. Day-count-annualised interest rates
#' map into the quarterly definition (`400 * quarterly flow / stock`) with
#' weights `4 * days / 365` (see [day_weights()]).
#' @param Fm Matrix (origins x 3q) of monthly forecasts (origin = a quarter's
#'   last month).
#' @param w Optional matrix of the same shape: weights summing (by quarter)
#'   to the quarterly conversion; `NULL` averages.
#' @return Matrix (origins x q).
#' @export
months_to_quarters <- function(Fm, w = NULL) {
  q <- ncol(Fm) %/% 3L
  sapply(seq_len(q), function(k) {
    j <- (3L * k - 2L):(3L * k)
    if (is.null(w)) rowMeans(Fm[, j, drop = FALSE]) else rowSums(Fm[, j, drop = FALSE] * w[, j, drop = FALSE])
  })
}

#' Day weights that map day-count-annualised monthly rates into quarterly ones
#' @param origin_months Month dates of the forecast origins.
#' @param h Months ahead.
#' @return Matrix (origins x h) of `4 * days / 365` for each forecast month.
#' @export
day_weights <- function(origin_months, h) {
  t(vapply(as.numeric(origin_months), function(o) {
    m <- seq(as.Date(o, origin = "1970-01-01"), by = "month", length.out = h + 1L)[-1]
    days_in_month(m) * 4 / 365
  }, numeric(h)))
}

#' Forecast errors against quarterly outcomes
#' @param Fq Matrix (origins x h) of quarterly forecasts.
#' @param origin_dates Quarter-end dates of the origins.
#' @param actual Data frame `date` (quarter end), `value`.
#' @return Long data frame `origin, h, forecast, actual, error`.
#' @export
quarterly_errors <- function(Fq, origin_dates, actual) {
  out <- list()
  for (i in seq_along(origin_dates)) for (k in seq_len(ncol(Fq))) {
    d <- next_quarters(origin_dates[i], k)[k]
    a <- actual$value[match(d, actual$date)]
    out[[length(out) + 1L]] <- data.frame(origin = origin_dates[i], h = k, forecast = Fq[i, k],
                                          actual = a, error = a - Fq[i, k])
  }
  r <- do.call(rbind, out)
  r[is.finite(r$error), , drop = FALSE]
}
