test_that("Kalman smoother and GLS give the same disaggregation, and the quarters add up", {
  set.seed(11)
  nm <- 180
  x <- cumsum(stats::rnorm(nm, 0.2, 1))
  u <- as.numeric(stats::arima.sim(list(ar = 0.7), nm, sd = 0.4))
  y <- 1 + 0.6 * x + u
  for (conv in c("sum", "average")) {
    c3 <- if (conv == "sum") 1 else 1 / 3
    yq <- colSums(matrix(y, 3)) * c3
    for (mod in c("chow-lin", "fernandez", "litterman")) {
      f <- td_disaggregate(yq, as.Date("2001-01-01"), X = cbind(x = x), conversion = conv, model = mod)
      expect_lt(max(abs(colSums(matrix(f$y, 3)) * c3 - yq)), 1e-9)
      expect_lt(max(abs(f$y - f$gls_y)), 1e-8)
    }
  }
  f <- td_disaggregate(colSums(matrix(y, 3)) / 3, as.Date("2001-01-01"), X = cbind(x = x),
                       conversion = "average", model = "chow-lin")
  expect_equal(unname(f$beta[2]), 0.6, tolerance = 0.1)
  expect_gt(stats::cor(f$y, y), 0.99)
})

test_that("the ragged edge is extrapolated with growing uncertainty", {
  set.seed(3)
  x <- cumsum(stats::rnorm(123))
  y <- 2 + 0.5 * x + as.numeric(stats::arima.sim(list(ar = 0.5), 123, sd = 0.3))
  yq <- colSums(matrix(y[1:120], 3)) / 3
  f <- td_disaggregate(yq, as.Date("2001-01-01"), X = cbind(x = x), conversion = "average")
  expect_length(f$y, 123)
  expect_lt(max(abs(f$y - f$gls_y)), 1e-8)
  expect_true(all(diff(f$se[121:123]) > 0))
})

test_that("smooth disaggregation of a quarterly average reproduces the quarters", {
  q <- data.frame(date = seq(as.Date("2008-01-01"), by = "3 months", length.out = 40),
                  value = 25 + cumsum(stats::rnorm(40, 0, 0.3)))
  m <- td_smooth_monthly(q, "2008-01-01", "2017-12-01")
  qa <- colMeans(matrix(m$value, 3))
  expect_lt(max(abs(qa - q$value)), 1e-8)
})

test_that("derived drivers use the frequency's windows and partial sums add up", {
  m <- data.frame(date = seq(as.Date("2010-01-01"), by = "month", length.out = 30),
                  repo = c(rep(6, 10), rep(6.5, 10), rep(6.25, 10)), gov10y = 9, cpi_yy = 5, unemp = 30)
  d <- derive_drivers(m, "month")
  expect_equal(d$d_repo[13], 0.5)                     # month 13 vs month 10
  expect_equal(d$d4_repo[20], 0.5)                    # 12-month change (month 20 vs month 8)
  expect_equal(d$repo_pos + d$repo_neg, m$repo - m$repo[1])
  expect_equal(d$prime, m$repo + 3.5)
  q <- derive_drivers(m[1:8, ], "quarter")
  expect_equal(q$d_repo[2], 0)
})

test_that("simulated dynamic multipliers match the ARDL formula", {
  n <- 60
  pan <- data.frame(date = quarter_end(seq(as.Date("2010-01-01"), by = "3 months", length.out = n)),
                    y = 1, repo = 6, gov10y = 9, cpi_yy = 5, unemp = 30, gdp_yy = 2, hpi_yy = 3,
                    zar_yy = 0, alsi_yy = 5, coinc_yy = 1, lead_yy = 1)
  pan <- derive_drivers(pan, "quarter")
  sat <- list(target = "y", ar = TRUE, spec = list(d = "repo", l = 1L),
              fit = list(coef = c(`(Intercept)` = 0.2, ar1 = 0.6, repo_L1 = 0.3)), bma = NULL, controls = character(0))
  class(sat) <- "baeconstrat_satellite"
  m <- satellite_dynamic_multiplier(sat, pan, list(repo = 1), n = 8, freq = "quarter")
  expect_equal(m$effect[1], 0)
  h <- 2:8
  expect_equal(m$effect[h], 0.3 * (1 - 0.6^(h - 1)) / (1 - 0.6), tolerance = 1e-10)
})

test_that("polynomial lags: weights and their sum", {
  pan <- data.frame(date = 1:40, y = stats::rnorm(40), x = stats::rnorm(40))
  dz <- pdl_design(pan, "y", "x", L = 4L, degree = 0L, ar = FALSE)
  expect_equal(unname(dz$X[10, "x_P0"]), sum(pan$x[6:10]))
  info <- dz$pdl$x
  expect_equal(pdl_weights(c(x_P0 = 0.2), info), rep(0.2, 5))
  s <- pdl_sum(c(x_P0 = 0.2), matrix(0.01, 1, 1, dimnames = list("x_P0", "x_P0")), info)
  expect_equal(unname(s["sum"]), 1)
  expect_equal(unname(s["se"]), 5 * 0.1)
})

test_that("shrinkage interpolates between OLS and zero", {
  set.seed(5)
  X <- cbind(`(Intercept)` = 1, x_L0 = stats::rnorm(100), x_L1 = stats::rnorm(100))
  y <- drop(X %*% c(1, 0.5, 0.2)) + stats::rnorm(100, 0, 0.3)
  free <- c(TRUE, FALSE, FALSE)
  tight <- shrinkage_fit(y, X, free, lambda = 1e8)
  loose <- shrinkage_fit(y, X, free, lambda = 1e-8)
  expect_lt(max(abs(tight$b[2:3])), 1e-4)
  expect_equal(unname(loose$b), unname(stats::lm.fit(X, y)$coefficients), tolerance = 1e-6)
  em <- shrinkage_fit(y, X, free)
  expect_true(is.finite(em$lambda) && em$lambda > 0)
})

test_that("months average into quarters", {
  Fm <- matrix(1:12, 2, 6, byrow = TRUE)
  expect_equal(months_to_quarters(Fm), cbind(c(2, 8), c(5, 11)))
})

test_that("the combined BA900 file keeps quarter ends (and the latest month) or every month", {
  root <- file.path(tempdir(), "cells_test")
  dir.create(file.path(root, "cells", "BA900"), recursive = TRUE, showWarnings = FALSE)
  mk <- function(p) data.frame(table_no = "6", table_desc = "x", item_no = c("0155", "0999"),
                               item_desc = "d", col_no = "0005", col_desc = "c", value = 1,
                               form = "BA900", period = p, inst_id = "A", inst_name = "A")
  for (p in c("2020-01-31", "2020-02-29", "2020-03-31", "2020-04-30"))
    saveRDS(mk(p), file.path(root, "cells", "BA900", paste0(p, "_A.rds")))
  q <- combine_form_cells("BA900", root, months = "quarter_end")
  expect_equal(sort(unique(format(q$period))), c("2020-03-31", "2020-04-30"))
  a <- combine_form_cells("BA900", root, months = "all", items = 155)
  expect_equal(length(unique(a$period)), 4L)
  expect_true(all(as.integer(as.character(a$item_no)) == 155L))
})

test_that("day counts and day weights", {
  expect_equal(days_in_month(as.Date(c("2024-02-10", "2023-02-01", "2025-12-31", "2025-04-01"))), c(29L, 28L, 31L, 30L))
  w <- day_weights(as.Date("2025-03-01"), 3)
  expect_equal(drop(w), c(30, 31, 30) * 4 / 365)
  expect_equal(sum(w), 91 * 4 / 365)
})
