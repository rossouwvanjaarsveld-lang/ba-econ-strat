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

test_that("the tranche hedge book reproduces the boxcar and reprices only maturing tranches", {
  bs <- toy_bs(); z <- toy_inputs(n = 12)
  z$rates$jibar3m <- z$rates$jibar
  z$rates$swap5y <- z$rates$jibar3m + seq(0.1, 0.6, length.out = 12)
  z$rates$swap7y <- z$rates$swap5y + 0.4
  hb <- list(notional = 300, tenor_q = 20, fixed0 = 8)
  ht <- c(hb, list(new_ref = "swap5y", new_tenor_q = 20))
  rb <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hb)
  rt <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = ht)
  expect_lt(max(abs(rb$quarterly$hedge_pl - rt$quarterly$hedge_pl)), 1e-10)
  ## reinvesting at a longer curve point adds carry in proportion to the
  ## share of the book that has matured
  h7 <- c(hb, list(new_ref = "swap7y", new_tenor_q = 28))
  r7 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = h7)
  d <- r7$quarterly$hedge_pl - rb$quarterly$hedge_pl
  expect_equal(d, cumsum(rep(300 / 20 * 0.4, 12)) / 100 / 4, tolerance = 1e-10)
  ## an extra tranche book struck now earns its fixed rate less JIBAR
  he <- c(ht, list(extra_notional = 40, extra_tenor_q = 20, extra_ref = "swap5y"))
  re <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = he)
  expect_equal(re$quarterly$hedge_pl[1] - rt$quarterly$hedge_pl[1],
               40 * (z$rates$swap5y[1] - z$rates$jibar[1]) / 100 / 4, tolerance = 1e-10)
  ## an extra book can be struck at a given market rate instead (its first
  ## tranche matures at once and rolls into the curve)
  hx <- c(ht, list(extra_notional = 40, extra_tenor_q = 20, extra_rate = 9))
  rx <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hx)
  expect_equal(rx$quarterly$hedge_pl[1] - rt$quarterly$hedge_pl[1],
               (38 * (9 - z$rates$jibar[1]) + 2 * (z$rates$swap5y[1] - z$rates$jibar[1])) / 100 / 4, tolerance = 1e-10)
  ## a pay-fixed overlay struck now offsets the book at today's rate
  hn <- c(ht, list(extra_notional = -60, extra_tenor_q = 20, extra_ref = "swap5y"))
  rn <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hn)
  expect_equal(rn$quarterly$hedge_pl[1] - rt$quarterly$hedge_pl[1],
               -60 * (z$rates$swap5y[1] - z$rates$jibar[1]) / 100 / 4, tolerance = 1e-10)
  ## a lower target is reached by letting tranches mature: the book shrinks
  ## by one tranche a quarter until it reaches the target, then rolls
  hg <- c(ht, list(target_notional = 300 - 4 * 15))
  rg <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hg)
  jb <- z$rates$jibar
  expect_equal(rg$quarterly$hedge_pl[1:4], 15 * (19:16) * (8 - jb[1:4]) / 100 / 4, tolerance = 1e-10)
  expect_equal(rg$quarterly$hedge_pl[5], (225 * (8 - jb[5]) + 15 * (z$rates$swap5y[5] - jb[5])) / 100 / 4,
               tolerance = 1e-10)
})

test_that("a hedge path closes the gap each quarter, either way, at that quarter's rate", {
  bs <- toy_bs(); z <- toy_inputs(n = 12)
  z$rates$swap5y <- z$rates$jibar + seq(0.1, 0.6, length.out = 12)
  z$rates$swap7y <- z$rates$swap5y + 0.4
  hb <- list(notional = 300, tenor_q = 20, fixed0 = 8, new_ref = "swap5y", new_tenor_q = 20)
  r0 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hb)
  ## a flat path at the current notional is the rolling book
  rp <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = c(hb, list(target_path = rep(300, 12))))
  expect_lt(max(abs(rp$quarterly$hedge_pl - r0$quarterly$hedge_pl)), 1e-10)
  expect_equal(rp$quarterly$hedge_notional, rep(300, 12))
  ## cutting to 240 in the first quarter: one tranche matures (285 left) and a
  ## pay-fixed tranche of 45 is struck at the 5-year rate
  path <- c(240, rep(240, 5), rep(330, 6))
  rd <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = c(hb, list(target_path = path)))
  jb <- z$rates$jibar
  expect_equal(rd$quarterly$hedge_pl[1], (285 * (8 - jb[1]) - 45 * (z$rates$swap5y[1] - jb[1])) / 100 / 4,
               tolerance = 1e-10)
  expect_equal(rd$quarterly$hedge_notional, path, tolerance = 1e-10)
  ## the book's PV01 is positive and falls as the book is cut
  expect_true(all(rd$quarterly$hedge_pv01 > 0))
  expect_lt(rd$quarterly$hedge_pv01[1], rp$quarterly$hedge_pv01[1])
  ## per-quarter reinvestment tenors: 5 years for six quarters, then 7
  hv <- utils::modifyList(hb, list(new_ref = rep(c("swap5y", "swap7y"), each = 6),
                                   new_tenor_q = rep(c(20, 28), each = 6)))
  rv <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = hv)
  d <- rv$quarterly$hedge_pl - r0$quarterly$hedge_pl
  expect_equal(d, c(rep(0, 6), cumsum(rep(15 * 0.4, 6))) / 100 / 4, tolerance = 1e-10)
})

test_that("the hedge pays the floating benchmark where the rates carry one", {
  bs <- toy_bs(); z <- toy_inputs(n = 4)
  h <- list(notional = 300, tenor_q = 20, fixed0 = 8, new_ref = "swap5y", new_tenor_q = 20)
  r_j <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = h)
  z$rates$float3m <- z$rates$jibar + c(0, 0, 0.1619, 0.1619)
  r_f <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, hedge = h)
  expect_equal(r_f$quarterly$hedge_pl - r_j$quarterly$hedge_pl, -300 * c(0, 0, 0.1619, 0.1619) / 100 / 4,
               tolerance = 1e-10)
  ## lines priced off "float3m" fall back to JIBAR in frames without it
  expect_equal(line_rate(list(ref = "float3m", spread_fixed = 0.5), list(jibar3m = 7)), 7.5)
  expect_equal(line_rate(list(ref = "float3m", spread_fixed = 0.5), list(float3m = 6, jibar3m = 7)), 6.5)
})

test_that("payout can be a number per quarter or a rule on the CET1 ratio", {
  bs <- toy_bs(); z <- toy_inputs(n = 8)
  r7 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, payout = 0.7)
  rv <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, payout = rep(0.7, 8))
  expect_equal(rv$quarterly$equity, r7$quarterly$equity)
  q <- r7$quarterly
  expect_equal(q$dividends, 0.7 * pmax(q$net_income, 0))
  ## a rule that pays out only while CET1 stays above a trigger
  trig <- stats::median(q$cet1_ratio)
  seen <- numeric(0)
  rule <- function(t, st) { seen[t] <<- st$cet1; if (st$cet1 > trig) 0.7 else 0.3 }
  rr <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, payout = rule)
  expect_equal(seen[-1], utils::head(rr$quarterly$cet1_ratio, -1))
  expect_equal(rr$quarterly$dividends / pmax(rr$quarterly$net_income, 1e-12),
               ifelse(seen > trig, 0.7, 0.3), tolerance = 1e-10)
})
