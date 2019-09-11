test_that("efs returns a decomposable graph", {
  d <- tgp_dat[, sample(3:300, 6)]
  G <- fit_graph(d, type = "fwd", trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})

test_that("bwd returns a decomposable graph", {
  d <- tgp_dat[, sample(3:300, 6)]
  G <- fit_graph(d, type = "bwd", trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})

test_that("tree returns a decomposable graph", {
  d <- tgp_dat[, sample(3:300, 6)]
  G <- fit_graph(d, type = "tree", trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})
