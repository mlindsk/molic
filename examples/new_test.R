## setwd("/home/mads/Documents/phd/publications/molic/src")
load("/home/mads/Documents/phd/publications/molic/data/tgp_dat.rda")
load("/home/mads/Documents/phd/publications/molic/data/tgp_haps.rda")
source("/home/mads/Documents/phd/publications/molic/R/old_outlier_utils.R")
source("/home/mads/Documents/phd/publications/molic/R/outlier_model.R")
source("/home/mads/Documents/phd/publications/molic/R/efs_utils.R")
source("/home/mads/Documents/phd/publications/molic/R/efs.R")
source("/home/mads/Documents/phd/publications/molic/R/old_rip.R")

library(dplyr)
library(igraph)
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/rip.cpp")
Rcpp::sourceCpp("/home/mads/Documents/phd/publications/molic/src/outlier_utils.cpp")

## ---------------------------------------------------------
##                    SETTING UP DATA    
## ---------------------------------------------------------
## haps <- tgp_haps[1:2]
## df   <- tgp_dat
## df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
## A    <- as.matrix(df)

## ---------------------------------------------------------
##                  CONSTRUCTING TABLES
## ---------------------------------------------------------
## x <- efs_init(df)
## for (i in 1:300) {
##   x <- efs_step(as.data.frame(df), x)
## }

## RIP <- old_rip(x$G_A)
## Cs  <- RIP$C
## Ss  <- RIP$S
## nCs <- a_marginals(A, Cs)
## nSs <- a_marginals(A, Ss)

## y <- A[1, ]

## ---------------------------------------------------------
##                    BENCHMARKING
## ---------------------------------------------------------
library(microbenchmark)
## microbenchmark::microbenchmark(old_TY(y, nCs, nSs), TY(y, nCs, nSs))

## ---------------------------------------------------------
##                   BENCHMARKING SIMULATIONS
## ---------------------------------------------------------
set.seed(7)
## N <- 2000
## microbenchmark(sim_TY(A, nCs, nSs, N, ncores = 4),
##   old_sim_TY(df, nCs, nSs, N, ncores = 4),
##   times = 1
## )

## profvis::profvis(sim_TY(A, nCs, nSs, 1000, ncores = 1))
## profvis::profvis(old_sim_TY(df, nCs, nSs, 1000, ncores = 1))

## ---------------------------------------------------------
##                     as. HELPERS
## ---------------------------------------------------------

# NOW LOCATED IN efs_utils.R

## as_adj_lst <- function(A) {
##   Delta <- colnames(A)
##   apply(A, 2, function(r) Delta[r])
## }


## ---------------------------------------------------------
##         A BENCHMARKING STUDY ON DIFFERENT GRAPHS
##             AND DIFFERENT NUMBER OF SIMS
## ---------------------------------------------------------
make <- function(df, haps, nvars, nedges) {
  h  <- unlist(haps)[1:nvars]
  df <- df %>% filter(pop_meta == "EUR") %>% select(unname(h))
  A  <- as.matrix(df)
  x  <- efs_init(df)
  ncomp <- nrow(x$G_A) * (nrow(x$G_A) - 1) / 2
  df_ <- as.data.frame(df)
  for (i in 1:nedges) x <- efs_step(df_, x)
  adj <- as_adj_lst(x$G_A)
  RIP <- rip(adj); Cs  <- RIP$C; Ss  <- RIP$S
  nCs <- a_marginals(A, Cs); nSs <- a_marginals(A, Ss)
  out <- list(df = df, A = A, nCs = nCs, nSs = nSs, x = x, adj = adj, ncomp = ncomp)
  structure(out, class = "make")
}

make(tgp_dat, tgp_haps, 10, 10)

## ---------------------------------------------------------
##             NVARS = 50, NEDGES = 30
## ---------------------------------------------------------
x <- make(tgp_dat, tgp_haps, 70, 150)
as_adj_lst(x$x$G_A)
plot(x$x$G)

t <- Sys.time()
sim_TY(x$A, x$nCs, x$nSs, 1000, ncores = 4)
tt <- Sys.time() - t

q <- Sys.time()
old_sim_TY(x$df, x$nCs, x$nSs, 1000, nc = 4)
qq <- Sys.time() - q

tt
qq
as.numeric(qq) / as.numeric(tt)


## ---------------------------------------------------------
##             NVARS = 100, NEDGES = 150
## ---------------------------------------------------------
y <- make(tgp_dat, tgp_haps, 100, 150)
plot(y$y$G)

t <- Sys.time()
sim_TY(y$A, y$nCs, y$nSs, 5000, ncores = 4)
tt <- Sys.time() - t

q <- Sys.time()
old_sim_TY(y$df, y$nCs, y$nSs, 5000, nc = 4)
qq <- Sys.time() - q

tt
qq
as.numeric(qq) / as.numeric(tt)


## ---------------------------------------------------------
##                  FIXING RIP FUNCTION
## ---------------------------------------------------------
A <- make(tgp_dat, tgp_haps, 6, 5)
A$df
rip(A$adj)

efs(A$df)
