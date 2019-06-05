load("../data/tgp_dat.rda")
load("../data/tgp_haps.rda")
library(dplyr)
library(igraph)
library(molic)
haps <- tgp_haps[1:20]
df   <- tgp_dat
df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
df


X <- efs(df)
Y <- efs(df, mdl_type = "mdl2")
Z <- efs_aic(df)
plot(X$G, vertex.size = 0.1)
plot(Y$G, vertex.size = 0.1)
plot(Z$G, vertex.size = 0.1)
