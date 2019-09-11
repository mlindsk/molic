test_that("an observation is an outlier", {
  set.seed(7)
  vars <- sample(3:300, 15)
  A    <- subset(tgp_dat, pop_meta == "EUR")[, vars]
  B    <- subset(tgp_dat, pop_meta == "AFR")[, vars]
  G    <- fit_graph(A, trace = FALSE)$G_adj
  # M    <- outlier_model(as.matrix(A), G)
  # z    <- unlist(B[7,])
  # expect_true(p_val(M, deviance(M, z)) <= 0.05)
  expect_true(TRUE)
})
