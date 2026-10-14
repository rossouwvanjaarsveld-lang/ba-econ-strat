hyper_toy <- function() {
  pr <- c("cards", "nmd")
  ps <- c("(Intercept)", "contraction", "hiking", "easing", "kappa")
  d <- expand.grid(product = pr, param = ps, stringsAsFactors = FALSE)
  d$mu <- ifelse(d$param == "easing", -1, ifelse(d$param == "kappa", -0.2, 0.5))
  d$tau <- 1; d$se_mu <- 0.5; d$k <- 5
  d
}

test_that("a bank at the population mean sits at the 50th percentile", {
  h <- hyper_toy()
  par <- matrix(0, 2, 5, dimnames = list(c("cards", "nmd"), c("(Intercept)", "contraction", "hiking", "easing", "kappa")))
  for (p in rownames(par)) for (k in colnames(par)) par[p, k] <- h$mu[h$product == p & h$param == k]
  mkt <- data.frame(product = c("cards", "cards"), regime = c("easing", "hiking"), growth = c(9, 7), n = 10,
                    mean_all = 8, dev = c(1, -1))
  pos <- regime_positioning(par, h, mkt, c(cards = 8, nmd = 7))
  expect_equal(pos$percentile, c(0.5, 0.5))
  expect_equal(pos$ngfb_effect, pos$par)
  expect_true(all(pos$frontier > pos$par))
  r <- strategy_rules(pos)
  expect_true(all(r$action %in% c("Hold", "Push", "Defend", "Guard", "Let go", "Ride")))
})

test_that("the scenario scorecard adds up and is centred on the population", {
  h <- hyper_toy()
  par <- matrix(0, 1, 5, dimnames = list("cards", c("(Intercept)", "contraction", "hiking", "easing", "kappa")))
  for (k in colnames(par)) par["cards", k] <- h$mu[h$product == "cards" & h$param == k]
  reg <- c("hiking", "hiking", "easing", "easing", "neutral", "contraction")
  proj <- data.frame(g_cards = c(7, 7, 9, 9, 8, 1))
  sc <- scenario_scorecard(par, h, reg, proj, c(cards = 8), "cards")
  expect_equal(sc$percentile, 0.5)
  expect_equal(sc$ngfb_growth, sc$market_growth + sc$share_effect)
  expect_equal(sc$share_effect, sc$par)
  ## the share effect equals the Stage 2 projection rule
  expect_equal(sc$share_effect * 6, sum(share_growth(par["cards", ], reg, proj$g_cards, 8, drift = FALSE)))
})

test_that("product margins follow the funding, loss and capital arithmetic", {
  bs <- data.frame(line = c("cards", "beh_savings"), side = c("asset", "deposit"), balance = c(100, 50),
                   rw = c(0.5, 0), clr_mult = c(4, 0), runoff = c(0.01, 0.10), stringsAsFactors = FALSE)
  lr <- data.frame(date = as.Date("2027-03-31"), cards = 15, beh_savings = 6, wholesale_other = 8, hqla = 5)
  m <- product_margins(lr, clr = 1, bs = bs, products = c(cards = "cards", beh_savings = "nmd"),
                       tax = 0.25, cet1 = 0.12, coe = 0.15)
  cd <- m[m$product == "cards", ]
  pre <- 15 - 8 - 0.25 * 3 - 4 * 1
  cap <- 0.12 * 0.5 * (15 - 8 * 0.75)
  expect_equal(cd$contribution, (pre * 0.75 - cap) / 100)
  dp <- m[m$product == "nmd", ]
  expect_equal(dp$contribution, (8 - 6 - 0.10 * 3) * 0.75 / 100)
})

test_that("share balance paths compound sector growth and the share effect", {
  par <- c(`(Intercept)` = 5, contraction = 0, hiking = 2, easing = -1, kappa = 0)
  b <- share_balance_path(100, c(8, 8), par, c("hiking", "easing"), 8)
  expect_equal(b, 100 * exp(cumsum(c(10, 7) / 400)))
})

test_that("local projections recover a known impulse response", {
  set.seed(9)
  n <- 400
  x <- cumsum(stats::rnorm(n, 0, 0.25))
  y <- numeric(n)
  for (t in 2:n) y[t] <- 0.5 * y[t - 1] + 0.4 * (x[t] - x[t - 1]) + stats::rnorm(1, 0, 0.05)
  d <- data.frame(date = seq_len(n), y = y, x = x)
  lp <- local_projection(d, "y", "x", H = 4L, p = 1L)
  expect_equal(lp$beta[lp$h == 0], 0.4, tolerance = 0.1)
  expect_equal(lp$beta[lp$h == 2], 0.4 * 0.25, tolerance = 0.1)
})
