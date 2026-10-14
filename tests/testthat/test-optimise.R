test_that("positioning QP respects bounds and moves towards positive gradients", {
  skip_if_not_installed("quadprog")
  grad <- list(R0 = c(a = 0.15, b = 0.14), C0 = c(a = 0.13, b = 0.13), L0 = c(a = 1.3, b = 1.3),
               G = rbind(a = c(m1 = 0.004, m2 = -0.002), b = c(m1 = 0.003, m2 = -0.001)),
               Jc = rbind(a = c(m1 = -0.001, m2 = 0), b = c(m1 = -0.001, m2 = 0)),
               Jl = rbind(a = c(m1 = -0.02, m2 = 0), b = c(m1 = -0.02, m2 = 0)))
  mv <- data.frame(move = c("m1", "m2"), lower = c(-2, -2), upper = c(2, 2), cost = c(0.001, 0.001))
  s <- solve_positioning(grad, mv, weights = c(a = 0.5, b = 0.5), cet1_min = 0.12, lcr_min = 1,
                         reference = NULL)
  expect_true(all(s$x <= 2 + 1e-9 & s$x >= -2 - 1e-9))
  expect_gt(s$x[["m1"]], 0); expect_lt(s$x[["m2"]], 0)
  # a binding CET1 floor caps the m1 move
  s2 <- solve_positioning(grad, mv, weights = c(a = 1, b = 0), cet1_min = 0.1295, lcr_min = 1,
                          reference = NULL)
  expect_lte(s2$x[["m1"]], 0.5 + 1e-6)
})

test_that("with a reference scenario the status quo is optimal under it", {
  skip_if_not_installed("quadprog")
  grad <- list(R0 = c(baseline = 0.15, x = 0.14), C0 = c(baseline = 0.13, x = 0.13),
               L0 = c(baseline = 1.3, x = 1.3),
               G = rbind(baseline = c(m1 = 0.004, m2 = 0.002), x = c(m1 = 0.001, m2 = 0.003)),
               Jc = matrix(0, 2, 2, dimnames = list(c("baseline", "x"), c("m1", "m2"))),
               Jl = matrix(0, 2, 2, dimnames = list(c("baseline", "x"), c("m1", "m2"))))
  mv <- data.frame(move = c("m1", "m2"), lower = c(-2, -2), upper = c(2, 2), cost = c(0.001, 0.001))
  s0 <- solve_positioning(grad, mv, weights = c(baseline = 1), reference = "baseline")
  expect_equal(unname(s0$x), c(0, 0))
  # under x, m1 is worth 3 bps less and m2 1 bp more than under the baseline:
  # the unconstrained optimum is (G_x - G_base) / (2 * cost)
  sx <- solve_positioning(grad, mv, weights = c(x = 1), reference = "baseline")
  expect_equal(unname(sx$x), c(-0.003, 0.001) / 0.002, tolerance = 1e-6)
  expect_equal(sx$exec_cost, 0.5 * sum(sx$x^2 * 0.002), tolerance = 1e-12)
})

test_that("execution costs are quadratic in the move", {
  mv <- data.frame(move = "m", lower = -1, upper = 1, cost = 0.0008)
  K <- execution_cost_matrix(mv)
  expect_equal(0.5 * K[1, 1] * 1^2, 0.0008)
  expect_equal(0.5 * K[1, 1] * 2^2, 4 * 0.0008)
})

test_that("bigger-than-optimal moves give back the gain, cheaper execution scales it", {
  skip_if_not_installed("quadprog")
  ## a linear toy engine: ROE = R0_s + G_s x, so the full engine equals the QP's model
  G <- rbind(baseline = c(m1 = 0.004, m2 = 0.002), x = c(m1 = 0.001, m2 = 0.003),
             y = c(m1 = 0.006, m2 = 0.001))
  R0 <- c(baseline = 0.15, x = 0.14, y = 0.16)
  bs <- data.frame(line = c("a0", "a1", "a2", "equity"), side = c("asset", "asset", "asset", "equity"),
                   balance = c(50, 25, 25, 100), stringsAsFactors = FALSE)
  mv <- data.frame(move = c("m1", "m2"), from = c("a0", "a0"), to = c("a1", "a2"), unit = "assets",
                   lower = c(-5, -5), upper = c(5, 5), cost = c(0.001, 0.001), stringsAsFactors = FALSE)
  ctx <- list(bs = bs, moves = mv, hedge = NULL, run = function(b, h, s) {
    x <- b$balance[2:3] - bs$balance[2:3]               # percent of total assets (100)
    roe <- R0[[s]] + sum(G[s, ] * x)
    list(quarterly = data.frame(roe = rep(roe, 4), cet1_ratio = 0.13, lcr = 1.5, nim = 0.03, pbt = 1,
                                iea = 100, net_income = 100 * roe))
  })
  sc <- c("baseline", "x", "y")
  grad <- positioning_gradients(ctx, sc, step = 0.5)
  expect_equal(grad$G, G, tolerance = 1e-10)
  expect_equal(grad$Jni, 400 * G, tolerance = 1e-8)
  ag <- positioning_aggressiveness(ctx, grad, mv, "x", sc, c(baseline = 0.5, x = 0.25, y = 0.25),
                                   alpha = c(0.5, 1, 2), cost_scale = c(1, 0.5), bound_scale = 1,
                                   cet1_min = 0.1, lcr_min = 1, reference = "baseline")
  g <- function(a) ag$size$gain[ag$size$alpha == a & ag$size$realised == "x"]
  for (a in c(0.5, 2)) expect_lt(abs(g(a) - (2 * a - a^2) * g(1)), 1e-8)
  expect_lt(ag$size$gain[ag$size$alpha == 1 & ag$size$realised == "baseline"], 0)
  c1 <- ag$capacity[ag$capacity$cost_scale == 1 & ag$capacity$realised == "x", ]
  c2 <- ag$capacity[ag$capacity$cost_scale == 0.5 & ag$capacity$realised == "x", ]
  expect_equal(c2$m1, 2 * c1$m1, tolerance = 1e-6)
  expect_equal(c2$gain, 2 * c1$gain, tolerance = 1e-6)
  rg <- regret_matrix(ctx, grad, mv, sc, cet1_min = 0.1, lcr_min = 1, reference = "baseline")
  expect_equal(unname(rg$positions["baseline", ]), c(0, 0))
  expect_equal(rg$gain_ni[-1, ], 400 * rg$gain[-1, ], tolerance = 1e-6)
})
