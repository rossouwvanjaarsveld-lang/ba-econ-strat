sim_ardl <- function(n = 100, seed = 11) {
  set.seed(seed)
  d <- data.frame(date = seq(as.Date("2001-03-31"), by = "quarter", length.out = n))
  d$x1 <- cumsum(rnorm(n, 0, 0.3)); d$x2 <- rnorm(n)
  y <- numeric(n)
  for (t in 2:n) y[t] <- 1 + 0.5 * y[t - 1] + 0.8 * d$x1[t - 1] - 0.6 * d$x2[t] + rnorm(1, 0, 0.5)
  d$y <- y
  d
}

test_that("the asymmetric decomposition reconstructs the series", {
  d <- data.frame(x = c(5, 6, 6.5, 6, 4, 4.5))
  a <- asym_decompose(d, "x", "partial_sum")
  expect_equal(a$x - a$x_pos - a$x_neg, rep(5, 6))
  expect_true(all(diff(a$x_pos) >= 0) && all(diff(a$x_neg) <= 0))
  s <- asym_decompose(data.frame(dx = c(-1, 0.5, 0)), "dx", "split")
  expect_equal(s$dx_pos + s$dx_neg, c(-1, 0.5, 0))
})

test_that("Clark-West rejects equal accuracy when the larger model is genuinely better", {
  set.seed(4); n <- 120
  e_model <- rnorm(n, 0, 0.5); e_bench <- e_model + rnorm(n)    # the benchmark misses a signal
  cw <- clark_west(e_bench, e_model)
  expect_lt(cw[["p"]], 0.01)
  expect_lt(cw[["mspe_ratio"]], 1)
  dm <- dm_test(e_bench, e_model)
  expect_gt(dm[["stat"]], 0)
})

test_that("the bounds test finds a level relationship only where there is one", {
  set.seed(5); n <- 80
  x <- cumsum(rnorm(n)); y <- numeric(n)
  for (t in 2:n) y[t] <- y[t - 1] + 0.5 * (0.8 * x[t - 1] - y[t - 1]) + rnorm(1, 0, 0.3)
  b1 <- bounds_test(y, x, nsim = 400, seed = 1)
  expect_identical(b1$decision, "level relationship")
  expect_equal(b1$long_run, 0.8, tolerance = 0.15)
  b2 <- bounds_test(cumsum(rnorm(n)), x, nsim = 400, seed = 1)
  expect_false(identical(b2$decision, "level relationship"))
  expect_true(all(b1$bounds[, "I1"] > b1$bounds[, "I0"]))
})

test_that("pseudo-out-of-sample comparison favours the true drivers", {
  d <- sim_ardl()
  cmp <- spec_compare(d, "y", list(drivers = character(0), lags = integer(0)),
                      list(drivers = c("x1", "x2"), lags = c(1L, 0L)), h = 4L, min_train = 40L)
  expect_lt(cmp$ratio, 0.9)
  expect_lt(cmp$cw1[["p"]], 0.05)
  expect_gt(cmp$n_origins, 40)
})

test_that("the intercept correction as a level shift does not compound through the AR term", {
  d <- sim_ardl()
  s <- satellite_search(d, "y", c("x1", "x2"), signs = c(x1 = 1, x2 = -1), lags = 0:1, max_k = 2)
  shifted <- d
  last8 <- utils::tail(seq_len(nrow(d)), 8)
  shifted$y[last8] <- shifted$y[last8] - 2
  ic <- satellite_intercept_correction(s, shifted, k = 4)
  path <- data.frame(date = next_quarters(max(d$date), 40), x1 = utils::tail(d$x1, 1), x2 = 0)
  p0 <- satellite_project(s, shifted, path, method = "best")
  pl <- satellite_project(s, shifted, path, method = "best", ic_window = 4L, ic_mode = "level")
  pe <- satellite_project(s, shifted, path, method = "best", ic_window = 4L, ic_mode = "equation")
  expect_equal(pl$value - p0$value, rep(ic, 40), tolerance = 1e-10)
  rho <- s$fit$coef[["ar1"]]
  expect_equal(utils::tail(pe$value - p0$value, 1), ic / (1 - rho), tolerance = 1e-3)
})

test_that("every named bank has a profile and the roster logic keeps unpulled banks in Other", {
  expect_true(all(names(peer_groups()) %in% peer_group_profiles()$key))
  expect_true(all(setdiff(names(default_ba900_institutions()), "TOTAL") %in% peer_group_profiles()$key))
  b9 <- data.frame(inst_id = c("1", "2"), inst_name = c("ABSA BANK LTD", "CAPITEC BANK"),
                   period = as.Date(c("2020-03-31", "2020-03-31")))
  r <- peer_group_roster(b9)
  expect_setequal(r$group, c("Big Four", "Challengers"))
})
