tb_bs <- function() {
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
tb_inputs <- function(n = 6, repo = 7) {
  dates <- next_quarters(as.Date("2025-12-31"), n)
  list(rates = data.frame(date = dates, repo = repo, jibar = repo + 0.35, prime = repo + 3.5,
                          tbill = repo + 0.2, gov10y = 10, swap5y = 8.5),
       growth = data.frame(date = dates, loans = 1, hqla = 1, deposits = 1),
       credit = data.frame(date = dates, clr = 1),
       ratios = data.frame(date = dates, nir_assets = 1.5, opex_assets = 2.5))
}

test_that("an NII overlay adds to NII quarter by quarter", {
  bs <- tb_bs(); z <- tb_inputs()
  r0 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios)
  r1 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, overlay = data.frame(date = z$rates$date, nii = 2))
  expect_equal(r1$quarterly$nii[1] - r0$quarterly$nii[1], 2)
  expect_equal(r1$quarterly$overlay_nii, rep(2, 6))
  expect_equal(r1$quarterly$pbt[1] - r0$quarterly$pbt[1], 2)
})

test_that("realised gains go to profit and the books' mark-to-market to the HQLA stock", {
  bs <- tb_bs(); z <- tb_inputs()
  r0 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios)
  ov <- data.frame(date = z$rates$date, nii = 0, gains = c(0, -3, 0, 0, 0, 0), hqla_mtm = -10)
  r1 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, overlay = ov)
  expect_equal(r1$quarterly$nii[2], r0$quarterly$nii[2])                 # not interest income
  expect_equal(r1$quarterly$pbt[2] - r0$quarterly$pbt[2], -3)
  expect_equal(r1$quarterly$overlay_gains, ov$gains)
  ## Level 1 at market value: the stock falls by the mark-to-market, outflows do not move
  liq0 <- liquidity_ratios(r1$balances[, 2], bs)
  expect_equal(r1$quarterly$lcr[1], (liq0[["hqla"]] - 10) / (liq0[["outflows"]] - liq0[["inflows"]]))
  expect_equal(liquidity_ratios(bs$balance, bs, hqla_adj = 5)[["hqla"]], liquidity_ratios(bs$balance, bs)[["hqla"]] + 5)
})

test_that("released bills repay the plug and give up the full bill rate", {
  bs <- tb_bs(); z <- tb_inputs()
  s <- 0.8; tb <- z$rates$tbill
  lr <- data.frame(date = z$rates$date, hqla = s * tb)              # the line earns only on its earning share
  r0 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, line_rates = lr)
  rz <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, line_rates = lr, buffer = list(release = 0, scale = s))
  expect_equal(rz$quarterly$nii, r0$quarterly$nii)
  rr <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, line_rates = lr, buffer = list(release = 0.2, scale = s))
  ## the line is 20% smaller and the plug falls by the same amount
  expect_equal(unname(rr$balances["hqla", 3]), unname(0.8 * r0$balances["hqla", 3]))
  ## (the plug also carries the difference in retained earnings)
  expect_equal(unname(r0$balances["wholesale_other", 3] - rr$balances["wholesale_other", 3]),
               unname(0.2 * r0$balances["hqla", 3] + rr$balances["equity", 3] - r0$balances["equity", 3]), tolerance = 1e-8)
  a <- colSums(rr$balances[bs$side == "asset", ]); l <- colSums(rr$balances[bs$side != "asset", ])
  expect_true(all(abs(a - l) < 1e-8))
  ## once the release has settled, the HQLA line loses the bills' full yield on it
  ii_line <- function(r, t) r$lines["hqla", t]
  expect_equal(ii_line(r0, 3) - ii_line(rr, 3), 0.2 * 200 * tb[3] / 100 / 4, tolerance = 1e-8)
  expect_equal(rr$quarterly$plug_balance[3], unname(rr$balances["wholesale_other", 4]))
  ## fees, costs and non-credit RWA do not shrink with the released bills
  q0 <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, line_rates = lr, rwa_other = 0.1)$quarterly
  qr <- run_engine(bs, z$growth, z$rates, z$credit, z$ratios, line_rates = lr, rwa_other = 0.1,
                   buffer = list(release = 0.2, scale = s))$quarterly
  expect_equal(qr$opex, q0$opex); expect_equal(qr$nir, q0$nir)
  expect_equal(qr$rwa, q0$rwa)
  expect_error(run_engine(bs, z$growth, z$rates, z$credit, z$ratios, buffer = list(release = 0.9, scale = s)))
})

test_that("a duration book earns the curve over bills and marks to the 10-year yield", {
  z <- tb_inputs(n = 8)
  z$rates$float3m <- z$rates$jibar; z$rates$gov10y <- 10 + seq(0, 0.7, by = 0.1)
  jo <- list(gov10y = 10, float3m = z$rates$float3m[1])
  bb <- bond_book(rep(0.25, 8), 400, z$rates, jo, type = "duration", tax = 0.2)
  expect_equal(bb$held, rep(100, 8))
  expect_equal(bb$carry, 100 * (10 - z$rates$tbill) / 400)
  expect_equal(bb$oci[4], -bond_md(9, 10) * (z$rates$gov10y[4] - 10) / 100 * 100 * 0.8)
  expect_equal(bb$pv01[1], 1e-4 * bond_md(9.75, 10) * 100)
})

test_that("an asset swap earns the bond-swap spread and marks only to the spread", {
  z <- tb_inputs(n = 8)
  z$rates$float3m <- z$rates$jibar
  z$rates$gov10y <- 10 + seq(0, 0.7, by = 0.1)                       # the curve steepens 0.7 pp
  jo <- list(gov10y = 10, float3m = z$rates$float3m[1], bs = 0.6, gov5y = 9)
  sp <- asset_swap_spread(z$rates, jo)
  expect_equal(sp, 0.6 + seq(0, 0.7, by = 0.1))
  bb <- bond_book(rep(0.25, 8), 400, z$rates, jo, type = "asset_swap", tax = 0.2)
  expect_equal(bb$carry, 100 * (0.6 + z$rates$float3m - z$rates$tbill) / 400)
  expect_equal(bb$pv01, rep(0, 8))
  expect_true(all(bb$spv01 > 0))
  expect_equal(bb$oci[3], -bond_md(4.25, 9) * (sp[3] - 0.6) / 100 * 100 * 0.8)
  ## cutting the book realises the mark-to-market on what is sold
  bc <- bond_book(c(0.25, 0.25, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125), 400, z$rates, jo, type = "asset_swap", tax = 0)
  expect_equal(bc$held[3], 50)
  expect_equal(bc$oci[3], -bond_md(4.5, 9) * (sp[3] - 0.6) / 100 * 50 - bond_md(4.25, 9) * (sp[3] - 0.6) / 100 * 50,
               tolerance = 1e-10)
  ## the sale recycles its share of the mark-to-market to profit or loss; the
  ## rest stays in the OCI reserve
  expect_equal(bc$gains[3], -bond_md(4.5, 9) * (sp[3] - 0.6) / 100 * 50, tolerance = 1e-10)
  expect_equal(bc$gains[-3], rep(0, 7))
  expect_equal(bc$oci_unreal[3], -bond_md(4.25, 9) * (sp[3] - 0.6) / 100 * 50, tolerance = 1e-10)
  expect_equal(bc$mtm, bc$oci_unreal + cumsum(bc$gains), tolerance = 1e-10)
  bt <- bond_book(c(0.25, 0.25, 0.125, 0.125, 0.125, 0.125, 0.125, 0.125), 400, z$rates, jo, type = "asset_swap", tax = 0.2)
  expect_equal(bt$oci, 0.8 * bt$mtm); expect_equal(bt$oci_unreal, 0.8 * (bt$mtm - cumsum(bt$gains)))
  ov <- books_overlay(z$rates$date, list(bb, bc))
  expect_equal(ov$nii, bb$carry + bc$carry); expect_equal(ov$gains, bc$gains); expect_equal(ov$hqla_mtm, bb$mtm + bc$mtm)
  expect_equal(books_overlay(z$rates$date, list())$nii, rep(0, 8))
})

test_that("the bond-swap spread takes a share of the long bond's non-policy move", {
  z <- tb_inputs(n = 4)
  z$rates$float3m <- c(7.35, 6.35, 5.35, 5.35)                       # the benchmark falls two points
  z$rates$gov10y <- c(10, 9.8, 10.5, 10.5)
  jo <- list(gov10y = 10, float3m = 7.35, bs = 0.6, gov5y = 9)
  sp <- asset_swap_spread(z$rates, jo, beta = 0.2, kappa = 0.3)
  expect_equal(sp, 0.6 + 0.3 * ((z$rates$gov10y - 10) - 0.2 * (z$rates$float3m - 7.35)))
  ## a purely policy-driven move (the 10-year moving by beta of the benchmark) leaves it alone
  z2 <- z$rates; z2$gov10y <- 10 + 0.2 * (z2$float3m - 7.35)
  expect_equal(asset_swap_spread(z2, jo, beta = 0.2, kappa = 0.3), rep(0.6, 4))
  ## the default is the scenarios' own swap curve: every point of slope is spread
  expect_equal(asset_swap_spread(z$rates, jo), 0.6 + (z$rates$gov10y - z$rates$float3m) - (10 - 7.35))
  bb <- bond_book(rep(0.25, 4), 400, z$rates, jo, type = "asset_swap", tax = 0, spread = sp)
  expect_equal(bb$spread, sp)
  expect_equal(bb$oci[2], -bond_md(4.5, 9) * (sp[2] - 0.6) / 100 * 100, tolerance = 1e-10)
  expect_error(bond_book(rep(0.25, 4), 400, z$rates, jo, type = "asset_swap", spread = sp[1:3]))
})

test_that("the buffer release hits a target LCR", {
  bs <- tb_bs()
  lcr0 <- liquidity_ratios(bs$balance, bs)[["lcr"]]
  r <- buffer_release_for_lcr(bs, lcr0 * 0.8, scale = 0.9)
  expect_equal(attr(r, "lcr"), lcr0 * 0.8, tolerance = 1e-8)
  expect_gt(r, 0)
  expect_equal(as.numeric(buffer_release_for_lcr(bs, lcr0 * 1.1)), 0)
})
