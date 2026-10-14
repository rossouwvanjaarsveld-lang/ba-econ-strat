toy_bs <- function() {
  data.frame(
    line = c("loans", "hqla", "deposits", "wholesale_other", "equity"),
    side = c("asset", "asset", "deposit", "funding", "equity"),
    balance = c(800, 200, 700, 220, 80),
    ref = c("prime", "tbill", "deposit", "jibar", "none"),
    spread = c(0.01, 0, 0, 0.01, 0), alpha = c(0, 0, 0.01, 0, 0), beta = c(0, 0, 0.5, 0, 0),
    rw = c(0.75, 0, 0, 0, 0), clr_mult = c(1, 0, 0, 0, 0), runoff = c(0, 0, 0.1, 0.4, 0),
    hqla = c(0, 1, 0, 0, 0), asf = c(0, 0, 0.9, 0.5, 1), rsf = c(0.65, 0.05, 0, 0, 0),
    earning = c(TRUE, TRUE, TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
}
toy_inputs <- function(n = 4, repo = 7) {
  dates <- next_quarters(as.Date("2025-12-31"), n)
  list(rates = data.frame(date = dates, repo = repo, jibar = repo + 0.35, prime = repo + 3.5,
                          tbill = repo + 0.2, gov10y = 10, swap5y = 8.5),
       growth = data.frame(date = dates, loans = 1, hqla = 1, deposits = 1),
       credit = data.frame(date = dates, clr = 1),
       ratios = data.frame(date = dates, nir_assets = 1.5, opex_assets = 2.5))
}

test_that("engine reproduces hand-computed NII and keeps the balance sheet balanced", {
  bs <- toy_bs(); z <- toy_inputs()
  r <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, plug = "wholesale_other")
  q <- r$quarterly
  ii <- (800 * (0.105 + 0.01) + 200 * 0.072) / 4
  ie <- (700 * (0.01 + 0.5 * 0.07) + 220 * (0.0735 + 0.01)) / 4
  expect_equal(q$nii[1], ii - ie, tolerance = 1e-10)
  B <- r$balances
  a <- colSums(B[bs$side == "asset", ]); l <- colSums(B[bs$side != "asset", ])
  expect_true(all(abs(a - l) < 1e-8))
  expect_equal(q$impairments[1], 800 * 0.01 / 4)
})

test_that("a receive-fixed hedge gains when rates fall", {
  bs <- toy_bs()
  lo <- toy_inputs(repo = 5); hi <- toy_inputs(repo = 9)
  h <- list(notional = 300, tenor_q = 20, fixed0 = 8)
  rl <- run_engine(bs, lo$growth, lo$rates, lo$credit, lo$ratios, hedge = h)
  rh <- run_engine(bs, hi$growth, hi$rates, hi$credit, hi$ratios, hedge = h)
  expect_gt(rl$quarterly$hedge_pl[1], rh$quarterly$hedge_pl[1])
})

test_that("ROE scales with leverage and tax as expected", {
  bs <- toy_bs(); z <- toy_inputs()
  r <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, tax = 0, payout = 0)
  q <- r$quarterly
  expect_equal(q$net_income[1], q$pbt[1])
  expect_equal(q$roe[1], 4 * q$net_income[1] / ((80 + q$equity[1]) / 2))
})
