# =============================================================================
#  hierarchy.R -- partial pooling across banks and posterior-predictive draws
#
#  The NGFB series' aggregation rule: NGFB is a draw from the posterior
#  predictive of a hierarchical model fitted to the large South African banks;
#  only population-level hyperparameters are published (k-anonymity >= 4) and
#  bank effects are integrated out. Two tools implement that here:
#
#   * eb_pool(): random-effects (DerSimonian-Laird) pooling of per-bank
#     estimates -> population mean mu, between-bank variance tau^2, and
#     shrunken bank estimates.
#   * draw_logistic_normal(): the Post 1 composition machinery (additive
#     log-ratio space, ridge-regularised covariance, Mahalanobis-truncated draw)
#     generalised to any number of categories.
# =============================================================================

#' Random-effects pooling of bank-level estimates (DerSimonian-Laird)
#'
#' @param est Numeric vector of per-bank estimates.
#' @param se Their standard errors.
#' @return List: `mu` (population mean), `se_mu`, `tau2` (between-bank
#'   variance), `shrunk` (empirical-Bayes bank estimates), `k`, `Q`, `I2`.
#' @export
eb_pool <- function(est, se) {
  ok <- is.finite(est) & is.finite(se) & se > 0
  est <- est[ok]; se <- se[ok]; k <- length(est)
  if (k == 0L) return(list(mu = NA_real_, se_mu = NA_real_, tau2 = NA_real_,
                           shrunk = numeric(0), k = 0L, Q = NA_real_, I2 = NA_real_))
  w <- 1 / se^2
  mu_fe <- sum(w * est) / sum(w)
  Q <- sum(w * (est - mu_fe)^2)
  denom <- sum(w) - sum(w^2) / sum(w)
  tau2 <- if (k > 1L && denom > 0) max(0, (Q - (k - 1)) / denom) else 0
  wr <- 1 / (se^2 + tau2)
  mu <- sum(wr * est) / sum(wr)
  B <- if (tau2 > 0) tau2 / (tau2 + se^2) else rep(0, k)
  list(mu = mu, se_mu = sqrt(1 / sum(wr)), tau2 = tau2,
       shrunk = stats::setNames(mu + B * (est - mu), names(est)),
       k = k, Q = Q, I2 = if (Q > 0 && k > 1L) max(0, (Q - (k - 1)) / Q) else 0)
}

#' Posterior-predictive draw of a new bank's parameter vector
#'
#' Each parameter is drawn from `N(mu, tau^2 + se_mu^2)`; the joint draw is
#' accepted only if its standardised distance from the population mean is at
#' most `radius` (the "typical bank" truncation used for NGFB since Post 1).
#' @param pools Named list of [eb_pool()] results.
#' @param seed Integer seed; `NULL` leaves the RNG state alone.
#' @param radius Truncation radius in standard-deviation units.
#' @param max_tries Rejection-sampling cap.
#' @return Named numeric vector, with attribute `distance`.
#' @export
draw_posterior_predictive <- function(pools, seed = 20260622L, radius = 1, max_tries = 1e5) {
  if (!is.null(seed)) set.seed(seed)
  mu <- vapply(pools, function(p) p$mu, 0)
  sd <- vapply(pools, function(p) sqrt(p$tau2 + p$se_mu^2), 0)
  sd[!is.finite(sd)] <- 0
  for (i in seq_len(max_tries)) {
    z <- stats::rnorm(length(mu))
    if (sqrt(sum(z^2)) <= radius) {
      out <- mu + sd * z
      attr(out, "distance") <- sqrt(sum(z^2))
      return(out)
    }
  }
  stop("posterior-predictive draw: no acceptance within max_tries")
}

#' Additive log-ratio transform (last column is the reference)
#' @param p Matrix of shares (rows sum to one, all positive).
#' @return Matrix with one fewer column.
#' @export
alr <- function(p) {
  p <- as.matrix(p)
  log(p[, -ncol(p), drop = FALSE] / p[, ncol(p)])
}

#' Inverse additive log-ratio transform
#' @param x Matrix in alr coordinates.
#' @return Matrix of shares.
#' @export
inv_alr <- function(x) {
  e <- cbind(exp(as.matrix(x)), 1)
  e / rowSums(e)
}

#' Mahalanobis-truncated logistic-normal draw of a composition
#'
#' Generalises the Post 1 NGFB deposit-composition draw: fit a logistic-normal
#' to the banks' category shares (alr space), regularise the covariance with a
#' ridge towards its diagonal (few banks, many categories), and draw one new
#' composition within `radius` of the mean.
#' @param shares Matrix (banks x categories) of shares; zeros are floored.
#' @param seed RNG seed.
#' @param radius Mahalanobis truncation radius.
#' @param ridge Shrinkage weight towards the diagonal covariance.
#' @param floor Minimum share used before the log-ratio (avoids log(0)).
#' @return List: `draw` (named shares), `mean_share` (pooled mean
#'   composition), `mu`, `Sigma`, `distance`.
#' @export
draw_logistic_normal <- function(shares, seed = 20260622L, radius = 1, ridge = 0.2,
                                 floor = 1e-4) {
  S <- as.matrix(shares)
  S[S < floor] <- floor
  S <- S / rowSums(S)
  X <- alr(S)
  mu <- colMeans(X)
  C <- stats::cov(X)
  C[!is.finite(C)] <- 0
  C <- (1 - ridge) * C + ridge * diag(diag(C), nrow(C))
  diag(C) <- pmax(diag(C), 1e-8)
  L <- chol(C); Ci <- solve(C)
  if (!is.null(seed)) set.seed(seed)
  repeat {
    z <- stats::rnorm(length(mu))
    x <- mu + as.numeric(t(L) %*% z)
    d <- sqrt(as.numeric(t(x - mu) %*% Ci %*% (x - mu)))
    if (d <= radius) break
  }
  draw <- as.numeric(inv_alr(matrix(x, nrow = 1)))
  names(draw) <- colnames(S)
  list(draw = draw, mean_share = stats::setNames(as.numeric(inv_alr(matrix(mu, nrow = 1))), colnames(S)),
       mu = mu, Sigma = C, distance = d)
}
