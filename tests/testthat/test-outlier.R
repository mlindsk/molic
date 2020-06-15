test_that("an observation is an outlier", {
  set.seed(7)
  library(dplyr)
  vars <- sample(3:300, 15)
  eur  <- tgp_dat %>%
    filter(pop_meta == "EUR") %>%
    select(all_of(vars)) 

  z  <- tgp_dat %>%
    filter(pop_meta == "AFR") %>%
    select(vars) %>%
    slice(7) %>%
    unlist()

  g <- ess::fit_graph(eur, trace = FALSE)$G_adj
  m <- fit_outlier(eur, g, z)
  
  expect_true(m$pval <= 0.05)
})
