source("../R/load_all.R")
load("../data/tgp_dat.rda")
load("../data/tgp_haps.rda")
library(dplyr)
library(igraph)
haps <- tgp_haps
df   <- tgp_dat
df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))

t1 <- Sys.time()
gg <- haps[5]
efs(df[, unlist(gg)])
om <- outlier_model(df[, unlist(gg)], gg, 10000, 3)

t2 <- Sys.time()
t2 - t1

## fq <- function(x) df(x, 10, 1)
## curve(fq, 0, 11, add = TRUE)
## hist(om$sims + abs(min(om$sims)), freq = FALSE, add = TRUE)

pmf(om)
plot(om$G, vertex.size = 1)

t <- sapply(1:nrow(df), function(x) outlier_test(om, x))
s <- t <= 0.05
mean(s)
pmf(om)
outlier_test(om, 3)

TY(unlist(df[1, ]), om$C_marginals, om$S_marginals)

## library(microbenchmark)
## mb <- microbenchmark(mcs(G), mcs2(G_A), times = 10)
## ps1 <- mcs(G)
## ps2 <- mcs2(G_A)
## mb <- microbenchmark(perfect_sequence(G, ps1), perfect_sequence2(G_A, ps2), times = 10)
## mb <- microbenchmark(rip(G), rip2(G_A), times = 20)

## RIP  <- rip2(G_A)
## cliq <- RIP$C
## seps <- RIP$S
## Cms  <- a_marginals(df, cliq)
## Sms  <- a_marginals(df, seps)

## t1   <- Sys.time()
## simz <- simulate_cell(df, Cms, Sms, n.sim = 10000, ncores = 4)
## t2   <- Sys.time()
## t2 - t1

## simz <- simulate_cell(df, Cms, Sms, n.sim = 100, ncores = 1)
## simulate_TY(df, Cms, Sms, n.sim = 1000, ncores = 2)

## t1 <- Sys.time()
## ss <- simulate_cell(df, Cms, Sms, n.sim = 1000, ncores = 3)
## t2 <- Sys.time()
## t2 - t1

## t1 <- Sys.time()
## ss <- simulate_TY(df, Cms, Sms, n.sim = 100, ncores = 1)
## t2 <- Sys.time()
## t2 - t1

## pf <- profvis::profvis(simulate_cell(df, Cms, Sms, n.sim = 1000))
## pf

## tab   <- na(df, colnames(df)[2:6]); attr(tab, "vars"); tab
## na_ya(tab, unlist(df[1, 2:6]))
## na_b(tab, structure(c(1, 3), names = c("C", "T")))


## df   <- df %>% filter(pop_meta == "EUR") %>% select(contains("rs")) %>% select(1:30)
## tab   <- na(df, colnames(df)[2:6]); attr(tab, "vars"); tab
## na_ya(tab, unlist(df[1, 2:6]))

## na_b(tab, structure(c(1), names = c("C")))
## na_b(tab, structure(c(3,5), names = c("T", "A")))
## a_slice_b <- na_b(tab, structure(c(3,5), names = c("T", "A"))); a_slice_b
## na_ya(a_slice_b, c("C","T", "C"))

## G <- efs(df)$G
## RIP <- rip(G)
## cliq <- RIP$C
## seps <- RIP$S
## Cms <- a_marginals(df, cliq)
## Sms <- a_marginals(df, seps)
## y <- unlist(df[1, ])
## TY(y, Cms, Sms)

## simulate_cell(df, Cms, Sms, n.sim = 5)


## df2 <- df[,1:4]
## G <- efs(df2)$G
## RIP <- rip(G)
## cliq <- RIP$C
## seps <- RIP$S
## Cms <- a_marginals(df2, cliq)
## Sms <- a_marginals(df2, seps)
## y <- unlist(df2[1, ])
## simulate_cell(df2, Cms, Sms, n.sim = 500)
## library(doParallel)
## registerDoParallel(detectCores() - 1)
## res <- foreach(j = 1:5) %dopar% {
##   3*j
## }
## stopImplicitCluster()

## foreach::`%dopar%`(foreach(j = 1:5), {
##   3*j
## })
