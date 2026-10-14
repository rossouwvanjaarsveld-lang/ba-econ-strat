test_that("random-effects pooling collapses to the common value when banks agree", {
  p <- eb_pool(c(a = 1, b = 1, c = 1, d = 1), rep(0.2, 4))
  expect_equal(p$tau2, 0); expect_equal(p$mu, 1)
  q <- eb_pool(c(a = 0, b = 2, c = 4, d = 6), rep(0.1, 4))
  expect_gt(q$tau2, 0)
  expect_true(all(abs(q$shrunk - q$mu) <= abs(c(0, 2, 4, 6) - q$mu) + 1e-12))
})

test_that("logistic-normal draws are compositions, reproducible and truncated", {
  set.seed(9)
  S <- matrix(runif(24), 6, 4, dimnames = list(NULL, c("A", "B", "C", "D")))
  S <- S / rowSums(S)
  a <- draw_logistic_normal(S, seed = 20260622, radius = 1)
  b <- draw_logistic_normal(S, seed = 20260622, radius = 1)
  expect_equal(sum(a$draw), 1); expect_identical(a$draw, b$draw)
  expect_lte(a$distance, 1)
})

test_that("truncation radius reproduces Post 1 in three dimensions", {
  expect_equal(truncation_radius(3), 1)
  expect_gt(truncation_radius(10), truncation_radius(3))
})

test_that("alr and inv_alr are inverses", {
  p <- matrix(c(0.2, 0.3, 0.5, 0.1, 0.1, 0.8), 2, byrow = TRUE)
  expect_equal(inv_alr(alr(p)), p)
})

test_that("share-flow attribution conserves the gains", {
  ds <- c(A = 0.02, B = -0.015, C = -0.005, D = 0)
  M <- share_flow_matrix(ds)
  expect_equal(sum(M), 0.02); expect_equal(sum(M["A", ]), 0.02)
  expect_equal(M["A", "B"] / M["A", "C"], 3)
})
