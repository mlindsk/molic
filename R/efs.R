#' Efficient forward selection in decomposable graphical models
#' @description Efficient forward selection in decomposable graphical models
#' @param df data.frame
#' @param x A efs object
#' @param trace Logical indicating whether or not to trace the procedure
#' @param p Penalty term in the stopping criterion (\code{0} = AIC and \code{1} = BIC)
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy.
#' @return A efs object (a list) with values:
#' \describe{
#' \item{\code{G_adj}}{The fitted graph. Can be extracted with \code{as_adj_lst}}
#' \item{\code{G_A}}{The edge that was just deleted and the entropy attached to it. Can be extracted using \code{as_adj_mat}}
#' \item{\code{CG}}{The cliques of the graph.}
#' \item{\code{CG_A}}{A neighbor matrix of the clique graph.}
#' \item{\code{MSI}}{Minimal separator info list}
#' \item{\code{ht}}{An updated version of the input argument \code{ht}.}
#' }
#' @details The algorithm relies on calculating lots of entropies (see \url{https://arxiv.org/abs/1301.2267}). If \code{thres} is equal to a number, say \code{a}, the entropy will be calculated using the \code{table} function for all situations where the number of variables is less than or equal to \code{a}. Otherwise, the entropies will be calculated by pasting rows in the subset of \code{df} over the relevant variables. For small values of \code{a} the former method is generally faster whereas for large values of \code{a} it breaks down and it is necessary to use the latter method.
#' @examples
#' efs(tgp_dat[, 5:8])
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100} 
#' @seealso \code{\link{efs_init}}, \code{\link{efs_step}}, \code{\link{bws}}, \code{\link{bws_step}}, \code{\link{cl_tree}}, \code{\link{adj_list.efs}}, \code{\link{adj_matrix.efs}}
#' @export
efs <- function(df, x = efs_init(df), p = 0.5, trace = TRUE, thres = 5) {
  if (!("efs" %in% class(x)) ) stop("x is not a efs class")
  lv       <- sapply(df, function(x) length(unique(x)))
  n        <- ncol(df)
  M        <- nrow(df)
  complete <- n * (n-1L) / 2L
  k        <- sum(x$G_A)/2
  ## FIX THIS SO IT CAN HANDLE SINGLETONS!
  if ( n < 2 ) stop("df must have at least two variables")
  if ( p < 0 || p > 1 ) stop("p must be between 0 and 1")

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
#' @description Extracts the adjacency list of a \code{efs} or \code{bws} object
#' @param x \code{efs} or \code{bws} object
#' @return An adjacency list
#' @export
adj_list <- function(x) UseMethod("adj_list")

#' @rdname adj_list
#' @export
adj_list.efs <- function(x) x$G_adj

#' Adjacency Matrix
#' @description Extracts the adjacency matrix of a \code{efs} or \code{bws} object
#' @param x \code{efs} or \code{bws} object
#' @return An adjacency matrix
#' @export
adj_matrix <- function(x) UseMethod("adj_matrix")

#' @rdname adj_matrix
#' @export
adj_matrix.efs <- function(x) x$G_A
