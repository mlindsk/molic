## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
# For tracing the model selection procedure in git_graph
msg <- function(k, complete, val, stop_crit) {
  cat(paste(" Edges:", k, "of", complete, "-", stop_crit, "=", round(val, 6L)),"\n")
}

trivial <- function(x, null, complete) {
  # x: gengraph
  if( inherits(x, "bwd") ) return(null) 
  if( inherits(x, "fwd") ) return(complete)
}

update_iteration <- function(x) {
  # x: gengraph
  if( inherits(x, "bwd") ) {
    f <- function(k) return(k - 1L)
    return(f)
  }
  if( inherits(x, "fwd") ) {
    f <- function(k) return(k + 1L)
    return(f)
  }
}

stop_condition <- function(x) {
  # x: gengraph
  if( inherits(x, "bwd") ) {
    f <- function(stop_val) return( stop_val >= 0L)
    return(f)
  }
  if( inherits(x, "fwd") ) {
    f <- function(stop_val) return( stop_val <= 0L)
    return(f)
  }  
}

## ---------------------------------------------------------
##                      EXPORTED HELPERS
## ---------------------------------------------------------
#' Stepwise model selection
#' @description Stepwise model selection in decomposable graphical models
#' @param x \code{fwd} or \code{bwd} objects
#' @param df data.frame
#' @param q Penalty term in the stopping criterion  (\code{0} = AIC and \code{1} = BIC)
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @details A \code{bwd} (or \code{fwd}) object can be created using the \code{gengraph} constructor with \code{type = "bwd"}
#' @examples
#' d <- subset(digits, class == "1")[, 30:35]
#' gb <- gengraph(d, type = "bwd")
#' gf <- gengraph(d, type = "fwd")
#' wb <- walk(gb, d)
#' wf <- walk(gf, d)
#' # plot(wf)
#' # adj_mat(wf)
#' # adj_lst(wf)
#' @seealso \code{\link{fit_graph}}, \code{\link{walk.fwd}}, \code{\link{gengraph}}
#' @export
walk <- function(x, df, q, thres) UseMethod("walk")

#' Adjacency List
#' @description Extracts the adjacency list of a \code{gengraph}
#' @param x \code{gengraph}
#' @return An adjacency list
#' @export
adj_lst <- function(x) UseMethod("adj_lst")

#' @rdname adj_lst
#' @export
adj_lst.gengraph <- function(x) x$G_adj

#' Adjacency Matrix
#' @description Extracts the adjacency matrix of a \code{gengraph} object
#' @param x \code{gengraph} object
#' @return An adjacency matrix
#' @export
adj_mat <- function(x) UseMethod("adj_mat")

#' @rdname adj_mat
#' @export
adj_mat.gengraph <- function(x) x$G_A

#' Converts an adjacency matrix to an adjacency list
#'
#' @param A Adjacency matrix
#' @export
as_adj_lst <- function(A) {
  Delta <- colnames(A)
  out <- lapply(seq_along(Delta), function(r) {
    Delta[as.logical(A[, r])]
  })
  names(out) <- Delta
  out
}

#' Converts an adjacency list to an adjacency matrix
#'
#' @param adj Adjacency list
#' @export
as_adj_mat <- function(adj) {
  stopifnot(length(names(adj)) == length(adj))
  Delta <- names(adj)
  N     <- length(Delta)
  A     <- matrix(0L, nrow = N, ncol = N, dimnames = list(Delta, Delta))
  for( d in seq_along(Delta) ) {
    idx <- match(adj[[d]], Delta)
    A[idx, d] <- 1L
    A[d, idx] <- 1L
  }
  A
}

#' Print
#'
#' A print method for \code{gengraph} objects
#'
#' @param x A \code{gengraph} object
#' @param ... Not used (for S3 compatability)
#' @export
print.gengraph <- function(x, ...) {
  nv  <- ncol(x$G_A)
  ne  <- sum(x$G_A)/2
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$CG),
    paste0("\n  ", cls),
    "\n -------------------------\n"
  )
}

#' Print
#'
#' A print method for \code{tree} objects
#'
#' @param x A \code{tree} object
#' @param ... Not used (for S3 compatability)
#' @export
print.tree <- function(x, ...) print.gengraph(x, ...)
# Note: print.tree is needed to overwrite the tree method from package "cli".


#' Plot
#'
#' A wrapper around igraphs plot method for \code{gengraph} objects
#' @param x A \code{gengraph} object
#' @param ... Extra arguments. See the igraph package
#' @examples
#' d <- subset(digits, class == "1")[, 20:30]
#' g <- gengraph(d, type = "fwd")
#' plot(g)
#' G <- fit_graph(d)
#' plot(G)
#' @import igraph
#' @export
plot.gengraph <- function(x, ...) {
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



