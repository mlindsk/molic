msg <- function(k, complete, curr_mdl) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(curr_mdl, 6L)), "\n")

efs <- function(df, x = efs_init(df), d = 3, thres = 3, trace = TRUE, log_mdl = TRUE, mdl_type = "mdl1", method = "forward") {
  # INPUT:
  # df: dataframe
  # x: efs object. Default is the null-graph
  # d: number needed to encode a single paramter (see Altmueller)
  # thres: When the size of a set is larger than thres, entropy2 is used for speed
  # - For binary data the "optimal" value of thres is closer to 7
  mdl <- mdl_(mdl_type)
  n        <- ncol(df)
  complete <- n * (n-1L) / 2L
  k        <- length(igraph::E(x$G))
  if( k == complete ) stop("The graph is already complete!")
  prev_mdl <- mdl(x$G_A, df, d, thres)
  x        <- efs_step(df, x, thres)
  curr_mdl <- mdl(x$G_A, df, d, thres)
  k <- k + 1L
  if( curr_mdl > prev_mdl || k == complete) return(x)
  while( curr_mdl <= prev_mdl ) {
    ## print(delta_aic(x, sapply(df, function(x) length(unique(x)))))
    if( k == complete ) {
      if( trace ) msg(k, complete, curr_mdl)
      return(x)
    } 
    if( trace ) msg(k, complete, curr_mdl)
    x <- efs_step(df, x, thres)
    k <- k + 1L
    prev_mdl <- curr_mdl
    curr_mdl <- mdl(x$G_A, df, d, thres)
  }
  if( trace ) msg(k, complete, curr_mdl)
  return(x)
}

print.efs <- function(x, ...) {
  nv <- ncol(x$G_A)
  ne <- length(igraph::E(x$G))
  print(ne)
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$CG),
    "\n  <efs>",
    "\n -------------------------\n"
  )
}

## source("../R/load_all.R")
## load("../data/tgp_dat.rda")
## load("../data/tgp_haps.rda")
## library(dplyr)
## library(igraph)
## haps <- tgp_haps[1:10]
## df   <- tgp_dat
## df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
## df
## lv <- sapply(df, function(x) length(unique(x))) 
## y1 <- efs(df, trace = TRUE, mdl_type = "mdl1")
## y2 <- efs(df, trace = TRUE, mdl_type = "mdl2")
## # y3 <- efs(df, trace = TRUE, mdl_type = "aic")
## par(mfrow = c(1,2))
## plot(y1$G)
## plot(y2$G)
