# =============================================================================
#  disaggregate.R -- quarterly series to monthly with the Kalman filter
#
#  Real GDP, the unemployment rate and house prices are published quarterly.
#  A monthly model needs them monthly, and the monthly path must (i) add up
#  to the published quarter exactly and (ii) borrow its within-quarter shape
#  from monthly indicators where there are any. That is temporal
#  disaggregation (Chow and Lin, 1971; Fernandez, 1981; Litterman, 1983). In
#  state-space form (Harvey and Pierse, 1984; Proietti, 2006):
#
#     y_t = x_t' beta + u_t                       (monthly, unobserved)
#     u_t = phi u_{t-1} + e_t                     Chow-Lin
#     u_t = u_{t-1} + e_t                         Fernandez
#     u_t = u_{t-1} + n_t,  n_t = phi n_{t-1} + e_t   Litterman
#     Y_q = sum over the quarter's months of c * y_t  (observed, c = 1 or 1/3)
#
#  The quarterly observation is the end-of-quarter value of a "cumulator"
#  state; the months in between are missing observations, which the Kalman
#  filter handles by skipping the update. beta is estimated by GLS on the
#  quarterly data, phi by maximising the profile likelihood, and the monthly
#  path is the Kalman (Rauch-Tung-Striebel) smoother's estimate. The same
#  path has a closed form (the GLS best linear unbiased predictor); both are
#  computed, and the tests check that they agree.
# =============================================================================

.td_months <- function(dates_m) month_start(dates_m)

# aggregation matrix: quarters x months (months must start a quarter)
.td_C <- function(nm, nq, conv) {
  C <- matrix(0, nq, nm)
  for (q in seq_len(nq)) C[q, (3 * q - 2):(3 * q)] <- conv
  C
}

# Sigma %*% t(Cm) where Sigma = Var(u) / sigma^2 for the residual process.
# Chow-Lin: Sigma is Toeplitz, phi^|i-j| / (1 - phi^2). Fernandez and
# Litterman: Sigma = (L'L)^{-1} with L = H D lower triangular (D first
# differences with u_0 = 0, H the AR(1) filter), solved by triangular solves.
.td_SigmaCt <- function(n, model, phi, Ct) {
  if (model == "chow-lin")
    return((outer(seq_len(n), seq_len(n), function(i, j) phi^abs(i - j)) / (1 - phi^2)) %*% Ct)
  L <- diag(n); L[cbind(2:n, 1:(n - 1))] <- -1
  if (model == "litterman") {
    H <- diag(n); H[cbind(2:n, 1:(n - 1))] <- -phi
    L <- H %*% L
  }
  forwardsolve(L, backsolve(t(L), Ct))
}

# GLS fit and profile log-likelihood for one phi
.td_gls <- function(Y, X, C, model, phi) {
  SCt <- .td_SigmaCt(ncol(C), model, phi, t(C))
  V <- C %*% SCt
  Vi <- chol2inv(chol(V))
  CX <- C %*% X
  A <- t(CX) %*% Vi
  beta <- solve(A %*% CX, A %*% Y)
  e <- Y - CX %*% beta
  n <- length(Y)
  s2 <- as.numeric(t(e) %*% Vi %*% e) / n
  ld <- 2 * sum(log(diag(chol(V))))
  list(beta = drop(beta), s2 = s2, ll = -0.5 * n * (log(2 * pi * s2) + 1) - 0.5 * ld,
       Vi = Vi, e = drop(e))
}

# maximise the profile likelihood over phi: coarse grid, then a local search
.td_fit <- function(Y, X, C, model, phi_range = c(0.01, 0.995)) {
  if (model == "fernandez") return(c(.td_gls(Y, X, C, model, 1), phi = 1))
  ll <- function(p) .td_gls(Y, X, C, model, p)$ll
  grid <- c(seq(phi_range[1], 0.9, length.out = 10), 0.95, 0.98, phi_range[2])
  v <- vapply(grid, ll, 0)
  k <- which.max(v)
  lo <- grid[max(1L, k - 1L)]; hi <- grid[min(length(grid), k + 1L)]
  o <- stats::optimize(ll, c(lo, hi), maximum = TRUE, tol = 1e-4)
  p <- if (o$objective > v[k]) o$maximum else grid[k]
  c(.td_gls(Y, X, C, model, p), phi = p)
}

#' Kalman filter and smoother for the disaggregation residual
#'
#' Runs the state-space form of the residual process with a cumulator state
#' that is observed (without noise) in the last month of each quarter.
#' @param z Quarterly aggregated residuals (`Y - C X beta`).
#' @param nm Number of months (may exceed `3 * length(z)`: months after the
#'   last quarter are extrapolated).
#' @param model `"chow-lin"`, `"fernandez"` or `"litterman"`.
#' @param phi Autoregressive parameter.
#' @param s2 Innovation variance.
#' @param conv Aggregation weight (1 = sum, 1/3 = average).
#' @return List: `u` (smoothed monthly residual), `se`, `loglik` (prediction
#'   error decomposition over the observed quarters).
#' @export
td_kalman_smoother <- function(z, nm, model, phi, s2, conv) {
  ## state: chow-lin / fernandez (u, s); litterman (u, n, s)
  lit <- model == "litterman"
  m <- if (lit) 3L else 2L
  iu <- 1L; isx <- m; ie <- if (lit) 2L else NA_integer_
  Tm <- function(first) {
    Tt <- matrix(0, m, m)
    if (lit) {
      Tt[ie, ie] <- phi                 # n_t = phi n_{t-1} + e
      Tt[iu, iu] <- 1; Tt[iu, ie] <- phi  # u_t = u_{t-1} + phi n_{t-1} + e
      Tt[isx, iu] <- conv; Tt[isx, ie] <- conv * phi
    } else {
      a <- if (model == "chow-lin") phi else 1
      Tt[iu, iu] <- a
      Tt[isx, iu] <- conv * a
    }
    Tt[isx, isx] <- if (first) 0 else 1
    Tt
  }
  R <- numeric(m); R[iu] <- 1; if (lit) R[ie] <- 1; R[isx] <- conv
  Q <- s2 * tcrossprod(R)
  a0 <- numeric(m)
  P0 <- matrix(0, m, m)
  if (model == "chow-lin") P0[iu, iu] <- s2 / (1 - phi^2)
  Zr <- numeric(m); Zr[isx] <- 1
  a_p <- matrix(0, nm, m); P_p <- array(0, c(m, m, nm))
  a_f <- matrix(0, nm, m); P_f <- array(0, c(m, m, nm))
  Ts <- vector("list", nm)
  a <- a0; P <- P0; ll <- 0
  for (t in seq_len(nm)) {
    Tt <- Tm(first = (t %% 3L) == 1L)
    Ts[[t]] <- Tt
    a <- drop(Tt %*% a); P <- Tt %*% P %*% t(Tt) + Q
    a_p[t, ] <- a; P_p[, , t] <- P
    q <- t / 3
    if ((t %% 3L) == 0L && q <= length(z) && is.finite(z[q])) {
      v <- z[q] - sum(Zr * a); F <- as.numeric(t(Zr) %*% P %*% Zr)
      K <- drop(P %*% Zr) / F
      a <- a + K * v; P <- P - tcrossprod(K) * F
      ll <- ll - 0.5 * (log(2 * pi * F) + v^2 / F)
    }
    a_f[t, ] <- a; P_f[, , t] <- P
  }
  ## Rauch-Tung-Striebel smoother
  a_s <- a_f; P_s <- P_f
  for (t in (nm - 1L):1L) {
    Pp <- P_p[, , t + 1L]
    J <- P_f[, , t] %*% t(Ts[[t + 1L]]) %*% MASS_ginv(Pp)
    a_s[t, ] <- a_f[t, ] + drop(J %*% (a_s[t + 1L, ] - a_p[t + 1L, ]))
    P_s[, , t] <- P_f[, , t] + J %*% (P_s[, , t + 1L] - Pp) %*% t(J)
  }
  list(u = a_s[, iu], se = sqrt(pmax(P_s[iu, iu, ], 0)), loglik = ll)
}

#' Temporal disaggregation of a quarterly series to months
#'
#' @param yq Quarterly values (oldest first), one per quarter starting at
#'   `start` (a month that begins a quarter).
#' @param start First month (Date) of the first quarter.
#' @param X Monthly indicator matrix (rows = months from `start`), without
#'   an intercept; `NULL` for none. May have more rows than `3 * length(yq)`:
#'   later months are extrapolated (the "ragged edge").
#' @param nm Number of months (default `nrow(X)` or `3 * length(yq)`).
#' @param conversion `"sum"` (flows) or `"average"` (rates, indices).
#' @param model `"chow-lin"`, `"fernandez"` or `"litterman"`.
#' @param phi_grid Range searched for the autoregressive parameter (ignored
#'   by Fernandez): a coarse grid, then a local maximisation of the profile
#'   likelihood.
#' @return List: `y` (monthly estimate), `se`, `beta`, `phi`, `sigma2`,
#'   `loglik`, `aic`, `gls_y` (the closed-form estimate, for checking),
#'   `months`, `model`.
#' @export
td_disaggregate <- function(yq, start, X = NULL, nm = NULL, conversion = c("sum", "average"),
                            model = c("chow-lin", "fernandez", "litterman"),
                            phi_grid = c(0.01, 0.995)) {
  conversion <- match.arg(conversion); model <- match.arg(model)
  conv <- if (conversion == "sum") 1 else 1 / 3
  nq <- length(yq)
  if (is.null(nm)) nm <- if (is.null(X)) 3L * nq else nrow(X)
  stopifnot(nm >= 3L * nq, all(is.finite(yq)))
  Xf <- cbind(`(Intercept)` = rep(1, nm), if (!is.null(X)) as.matrix(X)[seq_len(nm), , drop = FALSE])
  stopifnot(all(is.finite(Xf)))
  C <- .td_C(3L * nq, nq, conv)
  Xin <- Xf[seq_len(3L * nq), , drop = FALSE]
  fit <- .td_fit(yq, Xin, C, model, phi_range = range(phi_grid))
  phi <- fit$phi
  ## closed form (GLS best linear unbiased predictor), extended to all months
  Cfull <- cbind(C, matrix(0, nq, nm - 3L * nq))
  gls_y <- drop(Xf %*% fit$beta + .td_SigmaCt(nm, model, phi, t(Cfull)) %*% fit$Vi %*% fit$e)
  ## Kalman smoother on the aggregated residual
  ks <- td_kalman_smoother(fit$e, nm, model, phi, fit$s2, conv)
  y <- drop(Xf %*% fit$beta) + ks$u
  npar <- ncol(Xf) + 1L + as.integer(model != "fernandez")
  list(y = y, se = ks$se, beta = fit$beta, phi = phi, sigma2 = fit$s2, loglik = fit$ll,
       aic = -2 * fit$ll + 2 * npar, gls_y = gls_y,
       months = seq(month_start(start), by = "month", length.out = nm), model = model,
       conversion = conversion, kf_loglik = ks$loglik)
}

.quarter_start <- function(d) {
  d <- month_start(d)
  as.Date(sprintf("%s-%02d-01", format(d, "%Y"), (as.integer(format(d, "%m")) - 1L) %/% 3L * 3L + 1L))
}

.add_months <- function(d, k) {
  as.Date(vapply(d, function(x) as.numeric(seq(as.Date(x, origin = "1970-01-01"), by = "month",
                                                length.out = k + 1L)[k + 1L]), 0), origin = "1970-01-01")
}

#' Disaggregate quarterly real GDP to months with monthly indicators
#'
#' Log-levels with the average conversion (the mean of the three monthly log
#' levels equals the log of the quarter's monthly-average level, up to a
#' second-order term), then a pro-rata benchmark so the three months add up
#' to the published quarter exactly. Months after the last published quarter
#' for which the indicators exist are extrapolated (a nowcast).
#' @param gdp_q Data frame `date` (any day in the quarter), `value` (real GDP).
#' @param ind Data frame `date` (month) and one column per indicator (levels).
#' @param from First month (rounded down to the start of its quarter).
#' @param to Last month wanted (cut at the last month with every indicator).
#' @param model Disaggregation model.
#' @return List from [td_disaggregate()] plus `monthly` (data frame `date,
#'   gdp_m, se_log`), `indicators` and `n_quarters`.
#' @export
td_gdp_monthly <- function(gdp_q, ind, from, to, model = "chow-lin") {
  from <- .quarter_start(from); to <- month_start(to)
  q <- gdp_q[is.finite(gdp_q$value), c("date", "value")]
  q$qs <- .quarter_start(q$date)
  q <- q[order(q$qs), ]
  months <- seq(from, to, by = "month")
  X <- ind[match(months, month_start(ind$date)), setdiff(names(ind), "date"), drop = FALSE]
  cc <- stats::complete.cases(X)
  stopifnot(cc[1])
  last <- if (all(cc)) length(cc) else which(!cc)[1] - 1L     # stop at the first gap
  months <- months[seq_len(last)]; X <- X[seq_len(last), , drop = FALSE]
  q <- q[q$qs >= from & .add_months(q$qs, 2L) <= months[last], ]
  stopifnot(nrow(q) > 8L, identical(as.numeric(q$qs),
                                    as.numeric(seq(from, by = "3 months", length.out = nrow(q)))))
  fit <- td_disaggregate(log(q$value / 3), from, X = log(as.matrix(X)), nm = length(months),
                         conversion = "average", model = model)
  lv <- exp(fit$y)
  for (k in seq_len(nrow(q))) {
    ii <- (3L * k - 2L):(3L * k)
    lv[ii] <- lv[ii] * q$value[k] / sum(lv[ii])
  }
  fit$monthly <- data.frame(date = months, gdp_m = lv, se_log = fit$se)
  fit$indicators <- names(X)
  fit$n_quarters <- nrow(q)
  fit
}

#' Disaggregate a quarterly average (rate or index) to months without indicators
#'
#' A smooth monthly path whose quarterly averages reproduce the published
#' quarters: a Litterman model with an intercept only, in logs when
#' `log = TRUE`. Months after the last quarter (up to `to`) are extrapolated.
#' @param q Data frame `date`, `value` (quarterly).
#' @param from First month (rounded down to the start of its quarter).
#' @param to Last month.
#' @param log Work in log levels.
#' @param model Disaggregation model.
#' @return Data frame `date`, `value`, `se`.
#' @export
td_smooth_monthly <- function(q, from, to, log = FALSE, model = "litterman") {
  from <- .quarter_start(from); to <- month_start(to)
  q <- q[is.finite(q$value), c("date", "value")]
  q$qs <- .quarter_start(q$date)
  q <- q[q$qs >= from & .add_months(q$qs, 2L) <= to, ]
  q <- q[order(q$qs), ]
  q <- q[!duplicated(q$qs), ]
  ## keep the last run of consecutive quarters (e.g. the unemployment rate is
  ## semi-annual before the Quarterly Labour Force Survey began in 2008)
  gap <- c(FALSE, diff(as.numeric(q$qs)) > 95)
  q <- q[seq.int(max(c(1L, which(gap))), nrow(q)), ]
  stopifnot(nrow(q) > 8L, identical(as.numeric(q$qs),
                                    as.numeric(seq(q$qs[1], by = "3 months", length.out = nrow(q)))))
  months <- seq(q$qs[1], to, by = "month")
  yq <- if (log) base::log(q$value) else q$value
  fit <- td_disaggregate(yq, q$qs[1], X = NULL, nm = length(months), conversion = "average",
                         model = model)
  v <- if (log) exp(fit$y) else fit$y
  out <- data.frame(date = months, value = v, se = fit$se)
  attr(out, "fit") <- fit[c("phi", "sigma2", "loglik", "model")]
  out
}

#' Out-of-sample nowcast test of a GDP disaggregation model
#'
#' For each evaluation quarter, the model is estimated on the quarters before
#' it and the monthly indicators up to the end of it; the three months it
#' extrapolates give a nowcast of the quarter, compared with the published
#' value. A model whose monthly path carries real information about the
#' quarter should beat an AR(1) on quarterly growth, which sees no monthly
#' data.
#' @param gdp_q Data frame `date`, `value` (quarterly real GDP).
#' @param ind Data frame `date` + indicator columns (monthly levels); `NULL`
#'   or zero indicator columns for the no-indicator model.
#' @param from First month of the estimation window.
#' @param eval_from First evaluation quarter (Date in the quarter).
#' @param model Disaggregation model.
#' @return Data frame `quarter, actual, nowcast, ar1` (q/q growth, annualised
#'   percent).
#' @export
td_nowcast_eval <- function(gdp_q, ind, from, eval_from, model = "chow-lin") {
  from <- .quarter_start(from)
  q <- gdp_q[is.finite(gdp_q$value), c("date", "value")]
  q$qs <- .quarter_start(q$date)
  q <- q[q$qs >= from, ]; q <- q[order(q$qs), ]
  gq <- 400 * diff(log(q$value))
  ev <- which(q$qs >= .quarter_start(eval_from))
  out <- list()
  for (k in ev) {
    nm <- 3L * k
    months <- seq(from, by = "month", length.out = nm)
    X <- NULL
    if (!is.null(ind) && ncol(ind) > 1L) {
      X <- log(as.matrix(ind[match(months, month_start(ind$date)), setdiff(names(ind), "date"), drop = FALSE]))
      if (any(!is.finite(X))) next
    }
    yq <- log(q$value[seq_len(k - 1L)] / 3)
    fit <- tryCatch(td_disaggregate(yq, from, X = X, nm = nm, conversion = "average", model = model),
                    error = function(e) NULL)
    if (is.null(fit)) next
    now <- sum(exp(fit$y[(nm - 2L):nm]))
    ## AR(1) on quarterly growth, estimated on the same quarters
    g <- gq[seq_len(k - 2L)]
    b <- stats::lm.fit(cbind(1, g[-length(g)]), g[-1])$coefficients
    out[[length(out) + 1L]] <- data.frame(quarter = q$qs[k], actual = gq[k - 1L],
                                          nowcast = 400 * log(now / q$value[k - 1L]),
                                          ar1 = b[1] + b[2] * g[length(g)])
  }
  do.call(rbind, out)
}
