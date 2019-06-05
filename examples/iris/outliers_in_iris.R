## ---------------------------------------------------------
##                  SETTING UP DATA
## ---------------------------------------------------------
library(dplyr)
to_range <- function(x) {
  as.character(as.numeric(cut(x, 4, labels = 1:4)))
}
df <- iris %>%
  as_tibble() %>%
  mutate_if(is.double, to_range) %>%
  mutate(Species = as.character(Species))

## ---------------------------------------------------------
##                  OUTLIER DETECTION
## ---------------------------------------------------------
library(molic)
E <- efs(df, mdl_type = "mdl2")

dfs <- split(df, df$Species)
E   <- lapply(dfs, function(d) efs(d[, -5], mdl_type = "mdl2"))
plot(E$versicolor$G)
plot(E$setosa$G)
plot(E$virginica$G)


## ---------------------------------------------------------
##                   VERSICOLOR TABLE
## ---------------------------------------------------------
adjve <- E$versicolor$G_adj
RIPve <- rip(adjve)
Cve   <- RIPve$C
Sve   <- RIPve$S

Ave   <- dfs$versicolor %>% select(-Species) %>% as.matrix()
nCve  <- molic:::a_marginals(Ave, Cve)
nSve  <- molic:::a_marginals(Ave, Sve)
simve <- sim_TY(Ave, nCve, nSve, 10000, 1)
hist(simve)
cdfve <- ecdf(simve)

df_s  <- dfs$setosa %>% select(-Species)
df_vi <- dfs$virginica %>% select(-Species)



out_s_ve <- sapply(1:nrow(df_s), function(i) {
  si <- TY(unlist(df_s[i,]), nCve, nSve)
  1- cdfve(si) <= 0.05
})
mean(out_s_ve)

out_vi_ve <- sapply(1:nrow(df_vi), function(i) {
  vii <- TY(unlist(df_vi[i,]), nCve, nSve)
  1- cdfve(vii) <= 0.05
})
mean(out_vi_ve)
