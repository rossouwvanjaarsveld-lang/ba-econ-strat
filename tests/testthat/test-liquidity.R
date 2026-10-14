toy_depmat <- function() {
  # one quarter, two depositor classes, all six BA900 buckets
  d <- expand.grid(col = as.character(1:6), class = c("retail", "financial"), stringsAsFactors = FALSE)
  d$value <- c(10, 20, 30, 5, 15, 20,   0, 0, 40, 10, 10, 40)
  d$date <- as.Date("2025-06-30")
  d[, c("date", "class", "col", "value")]
}

test_that("deposit liquidity factors are balance-weighted Basel factors", {
  f <- deposit_liquidity_factors(toy_depmat())
  op <- f[f$line == "beh_operational", ]
  # operational = cheque (col 1) + <= 1 day (col 3): retail 10 + 30 at 10%,
  # financial 0 at 25% (cheque) + 40 at 100% (<= 1 day)
  expect_equal(op$runoff, (40 * 0.10 + 40 * 1.00) / 80)
  expect_equal(op$asf, (40 * 0.90 + 40 * 0) / 80)
  tm <- f[f$line == "beh_term", ]
  # term = 1-6 months (30% inside 30 days) + > 6 months (5% inside 30 days)
  expect_equal(tm$runoff, (15 * 0.1 * 0.3 + 20 * 0.1 * 0.05 + 10 * 1 * 0.3 + 40 * 1 * 0.05) / 85)
  expect_equal(sum(f$balance), sum(toy_depmat()$value))
})

test_that("liquidity ratios follow the LCR and NSFR definitions", {
  bs <- data.frame(line = c("loans", "hqla", "dep", "repo_funding", "equity"),
                   side = c("asset", "asset", "deposit", "funding", "equity"),
                   runoff = c(0.05, 0, 0.2, 0, 0), hqla = c(0, 1, 0, -1, 0),
                   asf = c(0, 0, 0.9, 0, 1), rsf = c(0.85, 0.05, 0, 0, 0))
  b <- c(700, 300, 800, 100, 100)
  r <- liquidity_ratios(b, bs)
  expect_equal(r[["hqla"]], 300 - 100)                       # repo encumbers its collateral
  expect_equal(r[["lcr"]], 200 / (160 - min(35, 0.75 * 160)))
  expect_equal(r[["nsfr"]], (720 + 100) / (595 + 15))
})

test_that("liquidity calibration reproduces the target ratios", {
  bal <- data.frame(line = c("loans", "hqla", "beh_operational", "beh_term", "wholesale_other", "equity"),
                    side = c("asset", "asset", "deposit", "deposit", "funding", "equity"),
                    balance = c(700, 300, 500, 300, 120, 80))
  ch <- data.frame(line = bal$line, runoff = c(0.02, 0, 0.3, 0.05, 0.25, 0), hqla = c(0, 1, 0, 0, 0, 0),
                   asf = c(0, 0, 0.6, 0.6, 0.5, 1), rsf = c(0.8, 0.05, 0, 0, 0, 0))
  cl <- calibrate_liquidity(bal, ch, lcr_target = 1.5, nsfr_target = 1.15)
  expect_equal(cl$check[["lcr"]], 1.5, tolerance = 1e-10)
  expect_equal(cl$check[["nsfr"]], 1.15, tolerance = 1e-10)
})
