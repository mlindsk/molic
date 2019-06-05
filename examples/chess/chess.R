## setwd("/home/mads/Documents/phd/publications/molic/src")
## load("/home/mads/Documents/phd/publications/molic/data/tgp_dat.rda")
## load("/home/mads/Documents/phd/publications/molic/data/tgp_haps.rda")
#source("/home/mads/Documents/phd/publications/molic/R/old_outlier_utils.R")
source("/home/mads/Documents/phd/publications/molic/R/outlier_model.R")
source("/home/mads/Documents/phd/publications/molic/R/efs_utils.R")
source("/home/mads/Documents/phd/publications/molic/R/efs.R")
## source("/home/mads/Documents/phd/publications/molic/R/old_rip.R")

library(dplyr)
library(igraph)
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/rip.cpp")
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/outlier_utils.cpp")


library(molic)
library(dplyr)
df <- read.csv("kr-vs-kp.data", header = FALSE)
A  <-as.matrix(df)
df_w <- df %>% filter(V37 == "won")   %>% select(-V37) %>% mutate_all(as.character)
df_l <- df %>% filter(V37 == "nowin") %>% select(-V37) %>% mutate_all(as.character)
efs_w <- efs_aic(df_w)
efs_l <- efs_aic(df_l)
Aw    <- as.matrix(df_w)
Al    <- as.matrix(df_l)
adjw  <- as_adj_lst(efs_w$G_A)
adjl  <- as_adj_lst(efs_l$G_A)
plot(efs_w$G)
plot(efs_l$G)


## ---------------------------------------------------------
##                   WINNER TABLE
## ---------------------------------------------------------
RIPw <- rip(adjw)
Cw   <- RIPw$C
Sw   <- RIPw$S
nCw  <- molic:::a_marginals(Aw, Cw)
nSw  <- molic:::a_marginals(Aw, Sw)
simw <- sim_TY(A, nCw, nSw, 10000, 4)
hist(simw)
sapply(1:nrow(df_l), function(i) {
  # print(i)
  TY(unlist(df_l[i,]), nCw, nSw)
})


## ---------------------------------------------------------
##                   LOSER TABLE
## ---------------------------------------------------------
RIPl <- rip(adjl)
Cl   <- RIPl$C
Sl   <- RIPl$S
nCl  <- molic:::a_marginals(Al, Cl)
nSl  <- molic:::a_marginals(Al, Sl)
siml <- sim_TY(A, nCl, nSl, 10000)
hist(siml)
sapply(1:nrow(df_w), function(i) {
  TY(unlist(df_w[i,]), nCl, nSl)
})

## ---------------------------------------------------------
##      FOR NOW WE CANT HANDLE UNSEEN CONFIGURATIONS!
## ---------------------------------------------------------
## FIX THE TY (or na_ya) FUNCTION!!!
## Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/outlier_utils.cpp")
## y <- unlist(df_w[111,])
## ny <- nCl[[1]]
## na_ya(ny, "ttasdasd fa dfg sdfg sdf g")
## TY(y, nCl, nSl)



