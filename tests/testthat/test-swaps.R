test_that("rbond files become quarterly averages, dropping thin quarters", {
  d <- tempfile(); dir.create(d)
  days <- seq(as.Date("2024-01-01"), as.Date("2024-07-10"), by = "day")
  days <- days[!format(days, "%u") %in% c("6", "7")]
  w <- function(id, v) utils::write.csv(data.frame(date = days, bond = id, yield_pct = v), file.path(d, paste0(id, ".csv")),
                                        row.names = FALSE)
  w("SASW5", seq(8, 7, length.out = length(days))); w("JIBAR3M", rep(7.5, length(days)))
  w("REPO", rep(7.25, length(days)))
  rb <- read_rbond_cache(d)
  expect_equal(nrow(rb), 2L)                       # 2024Q3 has too few fixings
  expect_equal(rb$jibar3m_rb, c(7.5, 7.5))
  expect_equal(rb$repo_rb, c(7.25, 7.25))
  q1 <- days < as.Date("2024-04-01")
  expect_equal(rb$swap5y[1], mean(seq(8, 7, length.out = length(days))[q1]))
})

test_that("the swap curve is JIBAR plus the average term spread; the book's rate averages actual and back-cast", {
  h <- data.frame(date = quarter_end(seq(as.Date("2021-01-01"), by = "3 months", length.out = 24)))
  h$jibar3m <- seq(4, 8, length.out = 24); h$gov10y <- h$jibar3m + 3
  h$gov5y <- h$jibar3m + 1.5
  h$swap5y <- ifelse(seq_len(24) > 14, h$gov5y - 1.2, NA)
  h$swap10y <- ifelse(seq_len(24) > 14, h$jibar3m + 1, NA)
  sp <- swap_term_spreads(h)
  expect_equal(sp$spread[sp$col == "swap5y"], 0.3, tolerance = 1e-12)
  s <- add_swap_curve(data.frame(jibar3m = c(6, 7)), h)
  expect_equal(s$swap5y, c(6.3, 7.3)); expect_equal(s$swap10y, c(7, 8))
  ## on a scenario the curve hangs off the floating benchmark (float3m)
  s2 <- add_swap_curve(data.frame(float3m = c(6, 7), jibar3m = c(1, 1)), h)
  expect_equal(s2$swap5y, c(6.3, 7.3))
  expect_equal(s$swap7y, s$swap5y + 0.4 * (s$swap10y - s$swap5y))
  f <- hedge_fixed0(h, tenor_q = 20L)
  expect_equal(f$n_actual, 10L); expect_equal(f$n_backcast, 10L)
  expect_equal(f$gov_spread, -1.2, tolerance = 1e-12)
  expect_equal(f$fixed0, mean(utils::tail(h$gov5y, 20) - 1.2), tolerance = 1e-12)
  ## without swap data the curve falls back to the government-curve proxy
  s0 <- add_swap_curve(data.frame(jibar3m = 6, gov10y = 9), h[, c("date", "jibar3m", "gov10y")])
  expect_equal(s0$swap5y, 6 + 0.55 * 3)
})
