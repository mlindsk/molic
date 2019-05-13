## setwd("/home/mads/Documents/phd/publications/molic/src")
## source("/home/mads/Documents/phd/publications/molic/old/R_old/outlier_utils.R")
## source("/home/mads/Documents/phd/publications/molic/old/R_old/efs_utils.R")
load("/home/mads/Documents/phd/publications/molic/data/tgp_dat.rda")
load("/home/mads/Documents/phd/publications/molic/data/tgp_haps.rda")
library(dplyr)
library(igraph)
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/outlier_utils.cpp")

haps <- tgp_haps[1:5]
df   <- tgp_dat
df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
df
A <- as.matrix(df)

nc1 <- n_a(A[, 3:4])
nc2 <- n_a(A[, 4:5])
ns2 <- n_a(A[, 4, drop = FALSE])

Cs <- list(nc1, nc2)
Ss <- list(NULL, ns2)
y <- A[1, ]

## NOW BENCHMARK WITH THE OLD ONE!!!! ## 
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/outlier_utils.cpp")
TY(y, Cs, Ss)
