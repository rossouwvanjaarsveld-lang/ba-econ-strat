test_that("channel attribution adds up to the full-engine difference", {
  bs <- data.frame(
    line = c("loans", "hqla", "deposits", "wholesale_other", "equity"),
    side = c("asset", "asset", "deposit", "funding", "equity"),
    balance = c(800, 200, 700, 220, 80),
    ref = c("prime", "tbill", "deposit", "jibar", "none"),
    spread = c(0.01, 0, 0, 0.01, 0), alpha = c(0, 0, 0.01, 0, 0), beta = c(0, 0, 0.5, 0, 0),
    rw = c(0.75, 0, 0, 0, 0), clr_mult = c(1, 0, 0, 0, 0), runoff = c(0, 0, 0.1, 0.4, 0),
    hqla = c(0, 1, 0, 0, 0), asf = c(0, 0, 0.9, 0.5, 1), rsf = c(0.65, 0.05, 0, 0, 0),
    earning = c(TRUE, TRUE, TRUE, TRUE, FALSE), stringsAsFactors = FALSE)
  mk <- function(repo, clr, g) {
    dates <- next_quarters(as.Date("2025-12-31"), 4)
    list(rates = data.frame(date = dates, repo = repo, jibar = repo + 0.35, prime = repo + 3.5,
                            tbill = repo + 0.2, gov10y = 10, swap5y = 8.5),
         growth = data.frame(date = dates, loans = g, hqla = 1, deposits = 1),
         credit = data.frame(date = dates, clr = clr),
         ratios = data.frame(date = dates, nir_assets = 1.5, opex_assets = 2.5),
         line_rates = data.frame(date = dates))
  }
  runner <- function(inp) run_engine(bs, inp$growth, inp$rates, inp$credit, inp$ratios,
                                     plug = "wholesale_other")
  a <- channel_attribution(mk(7, 1, 1.01), mk(8, 1.5, 1.00), runner)
  r <- a[a$metric == "roe", ]
  expect_equal(r$volume + r$pricing + r$credit + r$fees_costs + r$interaction, r$total)
  expect_equal(r$base + r$total, r$alt)
  expect_lt(r$credit, 0)                 # higher losses cost ROE
  expect_equal(r$fees_costs, 0)          # ratios unchanged
  ni <- a[a$metric == "net_income", ]
  expect_equal(ni$base + ni$total, ni$alt)
  expect_lt(ni$credit, 0)
})
