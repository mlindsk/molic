#' Backward selection in decomposable graphical models
#' @description Backward selection in decomposable graphical models
#' @param df data.frame
#' @param adj An adjacency list
#' @param p Penalty term in the stopping criterion  (\code{0} = AIC and \code{1} = BIC)
#' @param ht An environment (hashtable) containing precomputed entropies
#' @param trace Logical indicating whether or not to trace the procedure
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @return A bws object (a list) with values:
#' \describe{
#' \item{\code{G_adj}}{The fitted graph as an adjacency list.}
#' \item{\code{G_A}}{The fitted graph as an adjacency matrix.}
#' \item{\code{e}}{The edge that was just deleted and the entropy attached to it.}
#' \item{\code{S}}{The minimal separator that now separates the nodes in \code{e}.}
#' \item{\code{C}}{A list with the (maximal) cliques of the graph.}
#' \item{\code{ht}}{An updated version of the input argument \code{ht}.}
#' }
#' @details See \code{\link{efs}} for details about \code{thres}.
#' @examples
#' d <- tgp_dat[, 5:8]
#' bws(d, make_complete_graph(colnames(d)))
#' @seealso \code{\link{bws_step}}, \code{\link{make_complete_graph}}, \code{\link{efs}}, \code{\link{efs_step}}, \code{\link{cl_tree}} 
#' @export
bws <- function(df, adj, p = 0.5, ht = new.env(hash = TRUE), trace = TRUE, thres = 5) {
  x  <- bws_init(adj, ht)
  ## lv <- sapply(df, function(x) length(unique(x)))
  n  <- ncol(df)
  M    <- nrow(df)
  if ( n < 2 ) stop("df must have at least two variables")
  if ( p < 0 || p > 1 ) stop("p must be between 0 and 1")
  complete <- n * (n-1L) / 2L
  null     <- 0L
  k    <- sum(x$G_A)/2
  if (k == null) return(x)
  x  <- bws_step(df, x, thres)
  k <- k - 1L
  if (k == null) return(x)
  stop_val  <- attr(x$e, "ent") # delta_xic(x, lv, M, p)
  if (stop_val >= 0 ) return(x)
  while (stop_val < 0) {
    if (trace) msg(k, complete, stop_val, "xic")
    x <- bws_step(df, x, thres)
    k <- k - 1L
    if( k == null ) {
      if (trace) msg(k, complete, stop_val, "xic")
      return(x)
    }
    stop_val <- attr(x$e, "ent") # delta_xic(x, lv, M, p)
  } 
  return(x)
}

## df <- tgp_dat %>%
##   as_tibble() %>%
##   filter(pop_meta == "EUR") %>%
##   select(3:100) %>%
##   mutate_all(as.factor)

## E <- efs(df, p = 1)
## E2 <- E
## for (i in 1:100 ) E2 <- efs_step(df, E2)
## B <- bws(df, E2$G_adj, p = 0)

## par(mfrow = c(1,2))
## plot(E, vertex.size = 1)
## plot(B, vertex.size = 1)

#' Print bws
#'
#' A print method for \code{bws} objects
#'
#' @param x A \code{efs} object
#' @param ... Not used (for S3 compatability)
#' @export
print.bws <- function(x, ...) {
  nv <- ncol(x$G_A)
  ne <- sum(x$G_A)/2 # length(igraph::E(x$G))
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$C),
    "\n  <efs>",
    "\n -------------------------\n"
  )
}

#' Plot bws
#'
#' A wrapper around igraphs plot method for \code{bws} objects
#' @param x A \code{bws} object
#' @param ... Extra arguments. See the igraph package
#' @import igraph
#' @export
plot.bws <- function(x, ...) {
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

#' @rdname adj_list
#' @export
adj_list.bws <- function(x) x$G_adj

#' @rdname adj_matrix
#' @export
adj_matrix.bws <- function(x) x$G_A
