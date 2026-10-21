test_that("the JIBAR transition constants are the published ones", {
  jt <- jibar_transition()
  expect_equal(jt$last_fixing, as.Date("2026-12-31"))
  expect_equal(jt$cas, 0.1619)
  expect_equal(jt$new_contracts_banned, as.Date("2026-05-01"))
})

test_that("benchmark spreads use the daily policy rate for ZARONIA where it exists", {
  h <- data.frame(date = quarter_end(seq(as.Date("2024-01-01"), by = "3 months", length.out = 10)))
  h$repo <- 7; h$jibar3m <- 7.05
  sp <- benchmark_spreads(h)
  expect_equal(sp[["jibar"]], 0.05, tolerance = 1e-12)
  expect_equal(sp[["zaronia"]], -0.13)                          # no ZARONIA: the fallback
  h$zaronia <- 6.9; h$repo_rb <- 7.04
  expect_equal(benchmark_spreads(h)[["zaronia"]], -0.14, tolerance = 1e-12)
  h$repo_rb <- NULL
  expect_equal(benchmark_spreads(h)[["zaronia"]], -0.10, tolerance = 1e-12)
})

test_that("the benchmark switches to compounded ZARONIA + CAS after the last fixing", {
  d <- quarter_end(seq(as.Date("2026-07-01"), by = "3 months", length.out = 4))
  fb <- float_benchmark(d, rep(7, 4), c(jibar = 0.02, zaronia = -0.145, cas = 0.1619))
  expect_equal(fb$benchmark, c("JIBAR", "JIBAR", "ZARONIA + CAS", "ZARONIA + CAS"))
  expect_equal(fb$float3m, c(7.02, 7.02, 7 - 0.145 + 0.1619, 7 - 0.145 + 0.1619), tolerance = 1e-12)
  expect_equal(float_rate(list(jibar = 1)), 1)
  expect_equal(float_rate(data.frame(float3m = 2, jibar3m = 1)), 2)
})

test_that("equity growth never crosses a level break between two indices", {
  pr <- c(100, 102, 104, 106, 108, NA, NA, 114)
  fb <- c(50, 51, 52, 53, 54, 55, 56, 57)              # a different base
  g <- equity_growth(pr, fb, function(x) 100 * (log(x) - c(rep(NA, 4), utils::head(log(x), -4))))
  expect_equal(g[5], 100 * log(108 / 100))
  expect_equal(g[6], 100 * log(55 / 51))                # the fallback's own growth, not 55 / 102
  expect_equal(g[8], 100 * log(114 / 106))
})
