test_that("efs returns a decomposable graph", {
  G <- efs(tgp_dat[, sample(3:300, 6)], trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})
