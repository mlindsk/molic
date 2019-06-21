msg <- function(k, complete, val, stop_crit) {
  cat(paste(" Edges:", k, "of", complete, "-", stop_crit, "=", round(val, 6L)),"\n")
}
  

efs_mdl <- function(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl1", thres = 5) {
  lv       <- sapply(df, function(x) length(unique(x)))
  d        <- max(lv) # See Learning Bayesin Networks - An approach based on the MDL principle
  sf       <- stop_func(stop_crit)
  n        <- ncol(df)
  complete <- n * (n-1L) / 2L
  # k        <- length(igraph::E(x$G))
  k        <- sum(x$G_A)/2
  if (k == complete) stop("The graph is already complete!")
  prev_val <- sf(x$G_adj, lv, df, d, thres)
  x       <- efs_step(df, x, thres)
  curr_val <- sf(x$G_adj, lv, df, d, thres)
  k <- k + 1L
  if (curr_val > prev_val || k == complete) return(x)
  while (curr_val <= prev_val) {
    if (k == complete) {
      if (trace) msg(k, complete, curr_val, stop_crit)
      return(x)
    } 
    if (trace) msg(k, complete, curr_val, stop_crit)
    x <- efs_step(df, x, thres)
    k <- k + 1L
    prev_val <- curr_val
    curr_val <- sf(x$G_adj, lv, df, d, thres)
  }
  if (trace) msg(k, complete, curr_val, stop_crit)
  return(x)
}

efs_xic <- function(df, x = efs_init(df), trace = TRUE, stop_crit = "aic", thres = 5) {
  lv       <- sapply(df, function(x) length(unique(x)))
  sf       <- stop_func(stop_crit)
  n        <- ncol(df)
  M        <- nrow(df)
  complete <- n * (n-1L) / 2L
  # k        <- length(igraph::E(x$G))
  k        <- sum(x$G_A)/2
  if (k == complete) stop("The graph is already complete!")
  x     <- efs_step(df, x, thres)
  stop_val    <- sf(x, lv, M)
  k     <- k + 1L
  if (stop_val >= 0 || k == complete) return(x)
  while (stop_val < 0) {
    if (k == complete) {
      if (trace) msg(k, complete, stop_val, stop_crit)
      return(x)
    } 
    if (trace) msg(k, complete, stop_val, stop_crit)
    x_old <- x
    x  <- efs_step(df, x, thres)
    k  <- k + 1L
    stop_val <- sf(x, lv, M)
    if (stop_val >= 0) return(x_old)
  }
  if( trace ) msg(k, complete, stop_val, stop_crit)
  return(x)
}

#' efs
#'
#' Efficient forward felection in decomposable graphical models
#' 
#' @param df Dataframe
#' @param x An efs object
#' @param trace Logical indidcating whether or not to trace the procedure
#' @param stop_crit Stopping criterion (mdl1, mdl2, aic or bic)
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy
#' @export
efs <- function(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl1", thres = 5) {
  if( grepl("mdl", stop_crit) ) return(efs_mdl(df, x, trace, stop_crit, thres))
  else return(efs_xic(df, x, trace, stop_crit, thres))
}


#' Print efs
#'
#' A print method for \code{efs} objects
#'
#' @param x A \code{efs} object
#' @param ... Not used (for S3 compatability)
#' @export
print.efs <- function(x, ...) {
  nv <- ncol(x$G_A)
  ne <- sum(x$G_A)/2 # length(igraph::E(x$G))
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
## y1 <- efs(df, trace = TRUE, stop_crit = "mdl1")
## y2 <- efs(df, trace = TRUE, stop_crit = "mdl2")
## y3 <- efs(df, trace = TRUE, stop_crit = "aic")
## y4 <- efs(df, trace = TRUE, stop_crit = "bic")

## par(mfrow = c(2,2))
## plot(y4$G)
