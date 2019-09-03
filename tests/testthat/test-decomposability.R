test_that("efs returns a decomposable graph", {
  d <- tgp_dat[, sample(3:300, 6)]
  G <- efs(d, trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})

test_that("bws returns a decomposable graph", {
  d <- tgp_dat[, sample(3:300, 6)]
  G <- bws(d, make_complete_graph(colnames(d)), trace = FALSE)$G_adj
  expect_true(is_decomposable(G))
})
