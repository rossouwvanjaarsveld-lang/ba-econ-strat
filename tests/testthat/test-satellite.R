sim_panel <- function(n = 90, seed = 1) {
  set.seed(seed)
  d <- data.frame(date = seq(as.Date("2004-03-31"), by = "quarter", length.out = n))
  d$x1 <- cumsum(rnorm(n, 0, 0.3)); d$x2 <- rnorm(n); d$x3 <- rnorm(n)
  y <- numeric(n)
  for (t in 2:n) y[t] <- 1 + 0.5 * y[t - 1] + 0.8 * d$x1[t - 1] - 0.6 * d$x2[t] + rnorm(1, 0, 0.5)
  d$y <- y
  d
}

test_that("satellite search recovers the data-generating specification", {
  d <- sim_panel()
  s <- satellite_search(d, "y", c("x1", "x2", "x3"), signs = c(x1 = 1, x2 = -1, x3 = 1),
                        lags = 0:2, max_k = 3)
  expect_setequal(paste(s$spec$d, s$spec$l), c("x1 1", "x2 0"))
  m <- satellite_multipliers(s)
  expect_equal(m$beta[m$driver == "x1"], 0.8, tolerance = 0.3)
  expect_gt(s$skill, 0.2)
})

test_that("sign restrictions exclude wrong-signed drivers", {
  d <- sim_panel()
  s <- satellite_search(d, "y", c("x1", "x2"), signs = c(x1 = -1, x2 = 1), lags = 0L, max_k = 2)
  expect_length(s$spec$d, 0L)
  expect_match(s$note, "no macro specification")
})

test_that("projection with constant drivers converges to the long-run level", {
  d <- sim_panel()
  s <- satellite_search(d, "y", c("x1", "x2"), signs = c(x1 = 1, x2 = -1), lags = 0:1, max_k = 2)
  path <- data.frame(date = next_quarters(max(d$date), 60), x1 = 1, x2 = 0)
  p <- satellite_project(s, d, path)
  b <- s$fit$coef
  lr <- (b[["(Intercept)"]] + b[[grep("^x1", names(b), value = TRUE)]]) / (1 - b[["ar1"]])
  expect_equal(tail(p$value, 1), lr, tolerance = 1e-6)
})

test_that("HAC standard errors exceed OLS ones under positive autocorrelation", {
  set.seed(3); n <- 200
  x <- as.numeric(arima.sim(list(ar = 0.8), n)); e <- as.numeric(arima.sim(list(ar = 0.8), n))
  y <- 1 + 0.5 * x + e
  X <- cbind(`(Intercept)` = 1, x = x)
  f <- ols_hac(y, X)
  ols_se <- sqrt(diag(summary(lm(y ~ x))$cov.unscaled) * summary(lm(y ~ x))$sigma^2)
  expect_gt(f$se[["x"]], ols_se[[2]])
})

test_that("the intercept correction carries a recent level shift into the projection", {
  d <- sim_panel()
  s <- satellite_search(d, "y", c("x1", "x2"), signs = c(x1 = 1, x2 = -1), lags = 0:1, max_k = 2)
  shifted <- d
  last8 <- utils::tail(seq_len(nrow(d)), 8)
  shifted$y[last8] <- shifted$y[last8] - 2          # a location shift the model has not seen
  ic <- satellite_intercept_correction(s, shifted, k = 4)
  expect_lt(ic, -0.5)
  path <- data.frame(date = next_quarters(max(d$date), 4), x1 = utils::tail(d$x1, 1), x2 = 0)
  p0 <- satellite_project(s, shifted, path, method = "best")
  p1 <- satellite_project(s, shifted, path, method = "best", ic_window = 4L)
  expect_true(all(p1$value < p0$value))
})
