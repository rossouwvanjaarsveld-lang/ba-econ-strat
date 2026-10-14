mk_hist <- function() {
  d <- data.frame(date = seq(as.Date("2024-03-31"), by = "quarter", length.out = 10))
  d$date <- quarter_end(d$date)
  d$repo <- 7; d$jibar3m <- 7.35; d$gov10y <- 10; d$cpi_yy <- 4.5; d$gdp_yy <- 1
  d$unemp <- 32; d$hpi_yy <- 3; d$zar_yy <- 2; d$alsi_yy <- 8
  d
}
test_that("baseline converges to anchors and derived variables are consistent", {
  h <- mk_hist()
  b <- scenario_baseline(h, n = 40, half_life = 2)
  expect_equal(nrow(b), 40L)
  expect_equal(b$prime, b$repo + 3.5)
  expect_equal(tail(b$cpi_yy, 1), 3, tolerance = 1e-3)
  expect_equal(b$slope, b$gov10y - b$jibar3m)
})
test_that("scenario shocks are applied on top of the baseline", {
  h <- mk_hist(); b <- scenario_baseline(h, n = 12)
  s <- scenario_apply(b, list(repo = shock_path(12, 2, ramp = 2, hold = 2)), h, "test")
  expect_equal(s$repo[3] - b$repo[3], 2)
  expect_equal(s$prime - b$prime, s$repo - b$repo)
  lib <- scenario_library(b, h)
  expect_identical(names(lib), c("baseline", "upside", "downside"))
  expect_gt(max(lib$downside$repo), max(lib$baseline$repo))
  expect_lt(min(lib$downside$gdp_yy), min(lib$baseline$gdp_yy))
  expect_gt(max(lib$downside$cpi_yy), max(lib$baseline$cpi_yy))
  expect_lt(min(lib$upside$repo), min(lib$baseline$repo))
  expect_gt(mean(lib$upside$gdp_yy), mean(lib$baseline$gdp_yy))
})

test_that("the downside with cuts shares the real shock and reverses the policy response", {
  hk <- downside_shocks(12, "hike"); ct <- downside_shocks(12, "cut")
  for (v in c("gdp_yy", "unemp", "hpi_yy", "alsi_yy")) expect_equal(hk[[v]], ct[[v]])
  expect_gt(max(hk$repo), 0); expect_lt(min(ct$repo), 0)
  expect_gt(max(hk$cpi_yy), 0); expect_lt(min(ct$cpi_yy), 0)
  h <- mk_hist(); b <- scenario_baseline(h, n = 12)
  dc <- scenario_downside_cuts(b, h)
  expect_lt(min(dc$repo), min(b$repo))
})

test_that("partial sums of policy-rate changes continue from history", {
  h <- mk_hist(); h$repo <- c(7, 7.5, 8, 8, 7.5, 7, 7, 7.25, 7.25, 7)
  h <- asym_decompose(h, "repo", "partial_sum")
  b <- scenario_baseline(h, n = 8, repo_path = c(7.5, 8, 8, 7.5, 7, 7, 7, 7))
  expect_equal(b$repo - b$repo_pos - b$repo_neg, rep(h$repo[1], 8))
  expect_true(all(diff(c(utils::tail(h$repo_pos, 1), b$repo_pos)) >= 0))
  expect_true(all(diff(c(utils::tail(h$repo_neg, 1), b$repo_neg)) <= 0))
  expect_equal(b$gdp_neg, pmin(b$gdp_yy, 0))
})
