# =============================================================================
#  ols_hac.R -- OLS with Newey-West (HAC) inference, in base R
#
#  Quarterly bank aggregates are persistent and the regressions use
#  overlapping year-on-year macro transforms, so residuals are serially
#  correlated. Classical standard errors would overstate significance;
#  every t-statistic in the package is HAC.
# =============================================================================

#' Newey-West bandwidth rule of thumb
#' @param n Sample size.
#' @return Integer lag truncation `floor(4 (n/100)^(2/9))`.
#' @export
nw_bandwidth <- function(n) max(1L, as.integer(floor(4 * (n / 100)^(2 / 9))))

#' Newey-West HAC covariance of OLS coefficients (Bartlett kernel)
#' @param X Design matrix (n x k), including any intercept column.
#' @param e Residual vector (length n).
#' @param lag Truncation lag; defaults to [nw_bandwidth()].
#' @return k x k covariance matrix (with the n/(n-k) small-sample factor).
#' @export
hac_vcov <- function(X, e, lag = nw_bandwidth(nrow(X))) {
  n <- nrow(X); k <- ncol(X)
  XtXi <- tryCatch(solve(crossprod(X)), error = function(err) MASS_ginv(crossprod(X)))
  u <- X * as.numeric(e)
  S <- crossprod(u)
  if (lag > 0L && n > lag + 1L) {
    for (l in seq_len(lag)) {
      w <- 1 - l / (lag + 1)
      G <- crossprod(u[(l + 1L):n, , drop = FALSE], u[1L:(n - l), , drop = FALSE])
      S <- S + w * (G + t(G))
    }
  }
  V <- XtXi %*% S %*% XtXi * n / max(n - k, 1L)
  dimnames(V) <- list(colnames(X), colnames(X))
  V
}

# Moore-Penrose inverse without a MASS dependency
MASS_ginv <- function(A, tol = sqrt(.Machine$double.eps)) {
  s <- svd(A)
  pos <- s$d > max(tol * s$d[1L], 0)
  if (!any(pos)) return(array(0, dim(A)[2:1]))
  s$v[, pos, drop = FALSE] %*% ((1 / s$d[pos]) * t(s$u[, pos, drop = FALSE]))
}

#' OLS fit with HAC standard errors
#' @param y Numeric response.
#' @param X Numeric design matrix with column names.
#' @param hac_lag HAC truncation lag (default rule of thumb).
#' @return List: `coef`, `se`, `t`, `vcov`, `resid`, `fitted`, `sigma`, `r2`,
#'   `n`, `k`, `bic`.
#' @export
ols_hac <- function(y, X, hac_lag = NULL) {
  X <- as.matrix(X); y <- as.numeric(y)
  fit <- stats::lm.fit(X, y)
  b <- fit$coefficients
  b[is.na(b)] <- 0
  e <- as.numeric(y - X %*% b)
  n <- length(y); k <- ncol(X)
  V <- hac_vcov(X, e, lag = hac_lag %||% nw_bandwidth(n))
  se <- sqrt(pmax(diag(V), 0))
  tss <- sum((y - mean(y))^2)
  sig2 <- sum(e^2) / max(n - k, 1L)
  list(coef = b, se = se, t = ifelse(se > 0, b / se, NA_real_), vcov = V,
       resid = e, fitted = as.numeric(X %*% b), sigma = sqrt(sig2),
       r2 = if (tss > 0) 1 - sum(e^2) / tss else NA_real_,
       n = n, k = k, bic = n * log(sum(e^2) / n) + k * log(n))
}
