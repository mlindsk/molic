msg <- function(k, complete, val, stop_crit) {
  cat(paste(" Edges:", k, "of", complete, "-", stop_crit, "=", round(val, 6L)),"\n")
}
  

efs_mdl <- function(df, x = efs_init(df), trace = TRUE, thres = 5) {
  lv       <- sapply(df, function(x) length(unique(x)))
  d        <- max(lv) # See Learning Bayesin Networks - An approach based on the MDL principle
  n        <- ncol(df)
  complete <- n * (n-1L) / 2L
  k        <- sum(x$G_A)/2
  if (k == complete) stop("The graph is already complete!")
  prev_val <- mdl(x$G_adj, lv, df, d, thres)
  x        <- efs_step(df, x, thres)
  curr_val <- mdl(x$G_adj, lv, df, d, thres)
  k <- k + 1L
  if (curr_val > prev_val || k == complete) return(x)
  while (curr_val <= prev_val) {
    if (k == complete) {
      if (trace) msg(k, complete, curr_val, "mdl")
      return(x)
    } 
    if (trace) msg(k, complete, curr_val, "mdl")
    x <- efs_step(df, x, thres)
    k <- k + 1L
    prev_val <- curr_val
    curr_val <- mdl(x$G_adj, lv, df, d, thres)
  }
  if (trace) msg(k, complete, curr_val, "mdl")
  return(x)
}

efs_xic <- function(df, x = efs_init(df), trace = TRUE, stop_crit = "aic", thres = 5) {
  lv       <- sapply(df, function(x) length(unique(x)))
  n        <- ncol(df)
  M        <- nrow(df)
  complete <- n * (n-1L) / 2L
  # k        <- length(igraph::E(x$G))
  k        <- sum(x$G_A)/2
  if (k == complete) stop("The graph is already complete!")
  x     <- efs_step(df, x, thres)
  stop_val    <- delta_xic(x, lv, M, stop_crit)
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
    stop_val <- delta_xic(x, lv, M, stop_crit)
    if (stop_val >= 0) return(x_old)
  }
  if( trace ) msg(k, complete, stop_val, stop_crit)
  return(x)
}

#' Efficient forward selection in decomposable graphical models
#' @description Efficient forward selection in decomposable graphical models
#' @param df data.frame
#' @param x A efs object
#' @param trace Logical indicating whether or not to trace the procedure
#' @param stop_crit Stopping criterion ("mdl", "aic" or "bic")
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @return A efs object
#' @examples
#' ## TBA
#'
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100} 
#' @seealso \code{\link{efs_step}}, \code{\link{efs_adj_list.efs}}, \code{\link{efs_adj_matrix.efs}}
#' 
#' @export
efs <- function(df, x = efs_init(df), trace = TRUE, stop_crit = "mdl", thres = 5) {
  if( stop_crit == "mdl" ) return(efs_mdl(df, x, trace, thres))
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

efs_adj_list       <- function(x) UseMethod("efs_adj_list")

#' Adjacency List
#' @description Extracts the adjacency list of a \code{efs} object
#' @param x efs object
#' @return An adjacency list
#' @examples
#' ## TBA
#'
#' @seealso \code{\link{efs_adj_matrix.efs}}
#' @export
efs_adj_list.efs   <- function(x) x$G_adj

efs_adj_matrix     <- function(x) UseMethod("efs_adj_matrix")

#' Adjacency Matrix
#' @description Extracts the adjacency matrix of a \code{efs} object
#' @param x efs object
#' @return An adjacency matrix
#' @examples
#' ## TBA
#'
#' @seealso \code{\link{efs_adj_list.efs}}
#' @export
efs_adj_matrix.efs <- function(x) x$G_A


## library(dplyr)
## library(igraph)
## haps <- tgp_haps[1:3]
## df   <- tgp_dat
## df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
## y <- efs(df, trace = TRUE, stop_crit = "aic")
## igraph::graph_from_adjacency_matrix
## igraph::graph_from_adj_list
