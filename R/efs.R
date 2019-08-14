msg <- function(k, complete, val, stop_crit) {
  cat(paste(" Edges:", k, "of", complete, "-", stop_crit, "=", round(val, 6L)),"\n")
}

#' Efficient forward selection in decomposable graphical models
#' @description Efficient forward selection in decomposable graphical models
#' @param df data.frame
#' @param x A efs object
#' @param trace Logical indicating whether or not to trace the procedure
#' @param p Penalty term in the stopping criterion  (\code{0} = AIC and \code{1} = BIC)
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @return A efs object
#' @examples
#' efs(tgp_dat[, 5:8])
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100} 
#' @seealso \code{\link{cl_tree}}, \code{\link{efs_step}}, \code{\link{adj_list.efs}}, \code{\link{adj_matrix.efs}}
#' @export
efs <- function(df, x = efs_init(df), p = 0.5, trace = TRUE, thres = 5) {
  if (!("efs" %in% class(x)) ) stop("x is not a efs class")
  lv       <- sapply(df, function(x) length(unique(x)))
  n        <- ncol(df)
  ## FIX THIS SO IT CAN HANDLE SINGLETONS!
  if ( n < 2 ) stop("df must have at least two variables")
  if ( p < 0 || p > 1 ) stop("p must be between 0 and 1")
  M        <- nrow(df)
  complete <- n * (n-1L) / 2L
  k        <- sum(x$G_A)/2

  if (k == complete) return(x)
  x     <- efs_step(df, x, thres)
  k     <- k + 1L
  if (k == complete) return(x)

  stop_val    <- delta_xic(x, lv, M, p)
  if (stop_val >= 0 ) return(x)

  while (stop_val < 0) {
    if (trace) msg(k, complete, stop_val, "xic")
    x <- efs_step(df, x, thres)
    k <- k + 1L
    if (k == complete) {
      if( trace) msg(k, complete, stop_val, "xic")
      return(x)
    }
    stop_val <- delta_xic(x, lv, M, p)
  }
  
  if( trace ) msg(k, complete, stop_val, "xic")
  return(x)
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
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$CG),
    "\n  <efs>",
    "\n -------------------------\n"
  )
}

#' Plot efs
#'
#' A wrapper around igraphs plot method for \code{efs} objects
#' @param x A \code{efs} object
#' @param ... Extra arguments. See the igraph package
#' @import igraph
#' @export
plot.efs <- function(x, ...) {
  G      <- igraph::graph_from_adjacency_matrix(x$G_A, "undirected")
  args   <- list(...)
  args$x <- G
  if( is.null(args$vertex.frame.color) ) args$vertex.frame.color = "black"
  if( is.null(args$vertex.label.color) ) args$vertex.label.color = "black"
  if( is.null(args$vertex.color)       ) args$vertex.color       = "lightsteelblue2"
  if( is.null(args$vertex.size)        ) args$vertex.size        = 20
  if( is.null(args$vertex.label.cex)   ) args$vertex.label.cex   = 1
  # if( is.null(args$vertex.label.dist)  ) args$vertex.label.dist  = 2
  do.call("plot.igraph", args)
}

#' Adjacency List
#' @description Extracts the adjacency list of a \code{efs} object
#' @param x efs object
#' @return An adjacency list
#' @seealso \code{\link{adj_matrix.efs}}
#' @export
adj_list <- function(x) UseMethod("adj_list")

#' @rdname adj_list
#' @export
adj_list.efs <- function(x) x$G_adj

#' Adjacency Matrix
#' @description Extracts the adjacency matrix of a \code{efs} object
#' @param x efs object
#' @return An adjacency matrix
#' @seealso \code{\link{adj_list.efs}}
#' @export
adj_matrix <- function(x) UseMethod("adj_matrix")

#' @rdname adj_matrix
#' @export
adj_matrix.efs <- function(x) x$G_A


## library(dplyr)
## library(igraph)
## haps <- tgp_haps[1:3]
## df   <- tgp_dat
## df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
## y <- efs(df, trace = TRUE, stop_crit = "aic")
## igraph::graph_from_adjacency_matrix
## igraph::graph_from_adj_list
