test_that("an observation is an outlier", {
  set.seed(7)
  library(dplyr)
  vars <- sample(3:300, 15)
  eur  <- tgp_dat %>%
    filter(pop_meta == "EUR") %>%
    select(vars) 

  z  <- tgp_dat %>%
    filter(pop_meta == "AFR") %>%
    select(vars) %>%
    slice(7) %>%
    unlist()

  g <- fit_graph(eur, trace = FALSE)$G_adj
  m <- fit_outlier(as.matrix(eur), z, g)
  
  expect_true(m$pval <= 0.05)
})
