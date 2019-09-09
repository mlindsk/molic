## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
# Generic for methods step.fwd and step.bwd
step <- function(x, df, q, thres) UseMethod("step")

# For tracing the model selection procedure in git_graph
msg <- function(k, complete, val, stop_crit) {
  cat(paste(" Edges:", k, "of", complete, "-", stop_crit, "=", round(val, 6L)),"\n")
}

# x: gengraph object
is_graph_null              <- function(x) UseMethod("is_graph_null")
is_graph_complete          <- function(x) UseMethod("is_graph_complete")
is_graph_null.gengraph     <- function(x) length(x$CG) == length(x$G_adj)
is_graph_complete.gengraph <- function(x) length(x$CG) == 1L


rm_edge_lst <- function(adj, e) {
    # e is a 2-dim vector of chars
  adj[[e[1]]] <- setdiff(adj[[e[1]]], e[2])
  adj[[e[2]]] <- setdiff(adj[[e[2]]], e[1])
  return(adj)
}

rm_edge_mat <- function(A, e) {
  del_idx <- match(e, colnames(A))
  A[del_idx[1], del_idx[2]] <- 0L
  A[del_idx[2], del_idx[1]] <- 0L
  return(A)
}



## ---------------------------------------------------------
##                EXPORTED HELPERS
## ---------------------------------------------------------
#' Adjacency List
#' @description Extracts the adjacency list of a \code{gengraph}
#' @param g \code{gengraph}
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


#' Make a complete graph
#'
#' A helper function to make an adjacency list corresponding to a complete graph
#'
#' @param nodes A character vector containing the nodes to be used in the graph
#' @examples
#' d  <- tgp_dat[, 5:8]
#' cg <- make_complete_graph(colnames(d))
#' @export
make_complete_graph <- function(nodes) {
  structure(lapply(seq_along(nodes), function(k) {
    nodes[-which(nodes == nodes[k])]
  }), names = nodes)
}

#' Make a null graph
#'
#' A helper function to make an adjacency list corresponding to a null graph (no edges)
#'
#' @param nodes A character vector containing the nodes to be used in the graph
#' @export
make_null_graph <- function(nodes) {
  structure(lapply(seq_along(nodes), function(x) {
    character(0)
  }), names = nodes)
}


