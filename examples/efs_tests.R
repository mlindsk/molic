source("../R_old/load_all.R")
load("../data/tgp_dat.rda")
load("../data/tgp_haps.rda")
library(dplyr)
library(igraph)
haps <- tgp_haps[1:10]
df   <- tgp_dat
df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
df

## t1 <- Sys.time()
## E <- efs(df, trace = TRUE)
## save(E, file = "eds_all_eur.rda")
## t2 <- Sys.time() - t1
## t2

load(file = "eds_all_eur.rda")
plot(E$G, vertex.size = 1)
namz <- paste(names(unlist(haps)), "_", 1:ncol(df), sep = "")
Y <- E$G
Y <- set.vertex.attribute(Y, "name", value = namz)
plot(Y, vertex.size = 1)

comps <- igraph::components(Y)
memb  <- comps$membership
comps <- split(memb, memb)
names(comps) <- names(haps)
vsub  <- unname(unlist(lapply(comps, names)))
Gsub  <- igraph::induced_subgraph(Y, vsub)
Gsub_decomposition <- igraph::decompose.graph(Gsub)

print_decomp <- function() {
  readline(prompt = "")
  for (i in Gsub_decomposition ) {
    plot(i, vertex.size = 1)
    readline(prompt = "")
  }
}



# haps 1:30
# 1) 3 mins or so
# 2) introducing hash table for entropies
#    - 1.71 mins. 98 edges / 5886
# 3) removing redundant reversing
#    - now 57 secs

# 20 knuder ~ 6 sek.
# 50 knuder ~ 1.67 min


pf <- profvis::profvis(efs(df))
pf

## ----------- ##
## Akaike test ##
## ----------- ##
X <- efs_aic(df)
Y <- efs(df)
par(mfrow = c(1,2))
plot(X$G)
plot(Y$G)

## --------------------------------- ##
## HOW CLOSE TO A SIMULATED DATABASE ##
## --------------------------------- ##
## MAKE A GRAPH MYSELF AND INPUT!!!
## g != NULL
## ggg <- igraph::as.undirected(make_de_bruijn_graph(2, 2))

gg <- haps[1:10]
om <- outlier_model(df[, unlist(gg)], gg, 1000, 1)
Y  <- simulate_Y(df[, unlist(gg)], om$C_marginals, om$S_marginals, nsim = 1000)
plot(om$G)

Q <- matrix(nrow = 1000, ncol = length(unlist(gg)))
colnames(Q) <- unlist(gg)
for (i in seq_along(Y) ) {
  Q[i, ] <- Y[[i]]
}

Q <- as.data.frame(Q)
colnames(Q) <- unlist(gg)
rownames(Q) <- NULL

lapply(Q, function(x) class(x))
Q[] <- lapply(Q, function(x) as.character(x))

efs_init(Q)
plot(efs(Q)$G)

GG <- efs(as_tibble(Q))$G
FF <- efs_aic(as_tibble(Q))$G
par(mfrow = c(1,3))
plot(om$G)
plot(GG)
plot(FF)

## Local Variables:
## ess-r-package--project-cache: (molic . "~/Documents/phd/publications/molic/")
## End:
