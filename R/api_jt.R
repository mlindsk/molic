#' Junction Tree
#'
#' Construction of a junction tree and compilation of the network
#' 
#' @param g Either a decomposable undirected graph as a named list or a \code{gengraph} object or an \code{igraph} DAG
#' @param data A \code{data.frame} of characters. Note: all values must only be a single character. See details.
#' @param evidence A named vector
#' @param flow Character. Either "sum" or "max"
#' @param propagate Logical
#' @param validate Logical. See details.
#' @details It is assumed that all values in \code{data}, for all variables,
#' are represented as a single character. If \code{validate} is \code{TRUE} this is checked.
#' If values are not single characters, one may exploit the \code{to_single_chars} function.
#' @return A \code{jt} object
#' @seealso \code{\link{query_belief}}, \code{\link{mpe}}, \code{\link{get_cliques}}
#' @examples
#'
#' # Setting up the network
#' # ----------------------
#' library(igraph)
#' set.seed(7)
#' g <- make_tree(7)
#' g <- g + edge(4, 7)
#' g <- g + edge(5, 6)
#' g <- add_vertices(g, 3)
#' g <- g + edge(9, 10)
#' g <- set.vertex.attribute(g, "name", value = letters[1:10])
#' plot(g)
#'
#' # Some arbitrary data
#' d <- tgp_dat[1:500, 9:18]
#' colnames(d) <- letters[1:vcount(g)]
#' # -----------------------
#' 
#' # Example 1: sum-flow without evidence
#' jt1 <- jt(g, d)
#' plot(jt1)
#' print(jt1)
#' query_belief(jt1, c("a", "e", "g"))
#' query_belief(jt1, c("a", "d", "g"), type = "joint")
#'
#' # Example 2: sum-flow with evidence
#' e2  <- c(a = "T", b = "C")
#' jt2 <- jt(g, d, e2)
#' query_belief(jt2, c("a", "e", "g"))
#' query_belief(jt2, c("a", "d", "g"), type = "joint")
#' # Notice that, the configuration "TTT" has changed
#' # dramatically as a consequence of the evidence
#'
#' # We can get the probability of the evidence:
#' query_evidence(jt2)
#' 
#' # Example 3: max-flow without evidence
#' jt3 <- jt(g, d, flow = "max")
#' mpe(jt3)
#' 
#' # Example 4: max-flow with evidence
#' e4  <- c(a = "T", b = "C", c = "A", j = "A", g = "T")
#' jt4 <- jt(g, d, e4, flow = "max")
#' mpe(jt4)
#' 
#' # Notice, that the value of "d" has changed to "C" compared
#' # to max(jt3) where the value was "T" as a consequence
#' # of the new evidence
#'
#' # Example 5: investigating the clique graph before propagating
#' # TBA
#' 
#' @export
jt <- function(g, data, evidence = NULL, flow = "sum", propagate = TRUE, validate = TRUE) {
  jt <- new_jt(g, data, evidence, flow, validate)
  if (!propagate) return(jt)
  m <- send_messages(jt, flow)
  while (attr(m, "direction") != "FULL") m <- send_messages(m, flow)
  return(m)
}

#' Most Probable Explanation
#'
#' Returns the most probable configuration given the evidence entered in the junction tree
#' 
#' @param x A junction tree object, \code{jt}, with max-flow.
#' @seealso \code{\link{jt}}
#' @examples
#' # See the 'jt' function
#' @export
mpe <- function(x) UseMethod("mpe")

#' @rdname mpe
#' @export
mpe.jt <- function(x) {
  if(attr(x, "flow") != "max") stop("The flow of the junction tree is not 'max'.")
  return(attr(x, "mpe"))
}


#' Return the cliques of a junction tree
#'
#' @param x A junction tree object, \code{jt}.
#' @seealso \code{\link{jt}}
#' @export
get_cliques <- function(x) UseMethod("get_cliques")

#' @rdname get_cliques
#' @export
get_cliques.jt <- function(x) x$cliques


#' Query Evidence 
#'
#' Get the probability of the evidence entered in the junction tree object
#'
#' @param x A junction tree object, \code{jt}.
#' @examples
#' # See the 'jt' function
#' @seealso \code{\link{jt}}, \code{\link{mpe}}
#' @export
query_evidence <- function(x) UseMethod("query_evidence")

#' @rdname query_evidence
#' @export
query_evidence.jt <- function(x) {
  if(attr(x, "flow") != "sum") {
    stop("The flow of the junction tree must be 'sum'.")
  }
  if (attr(x, "direction") == "collect") {
    stop("In order to query the probabilty of evidence, ",
      "the junction tree must at least be propagted to ",
      "the root node.")
  }
  return(attr(x, "probability_of_evidence"))
}


#' Query probabilities
#'
#' Get probabilities from a junction tree object
#'
#' @param x A junction tree object, \code{jt}.
#' @param nodes The nodes for which the probability is desired
#' @param type Either 'marginal' or 'joint'
#' @examples
#' # See the 'jt' function
#' @seealso \code{\link{jt}}, \code{\link{mpe}}
#' @export
query_belief <- function(x, nodes, type = "marginal") UseMethod("query_belief")


#' @rdname query_belief
#' @export
query_belief.jt <- function(x, nodes, type = "marginal") {
  
  if (type %ni% c("marginal", "joint")) stop("Type must be 'marginal' or 'joint'.")
  
  if (attr(x, "flow") == "max") {
    stop("It does not make sense to query probablities from a junction tree with max-flow. ",
      "Use mpe(x) to obtain the max configuration.")
  }
  
  node_lst <- if (type == "joint") {
    list(nodes)
  } else {
    as.list(nodes)
  }
    
  .query <- lapply(node_lst, function(z) {

    # TODO: Also check the separators! They may be much smaller!!!
    # - especially for type = "marginal"!
    in_which_cliques <- .map_lgl(x$cliques, function(clq) all(z %in% clq))
    
    if (!any(in_which_cliques) && type == "joint") {
      stop("The function does not, at the moment, support queries of ",
        "nodes that belong to different cliques. ",
        "Use plot(x) or get_cliques(x) to see ",
        "the cliques of the junction tree."
      )
    }

    index_in_which_cliques <- which(in_which_cliques)
    length_of_possible_cliques <- .map_int(x$cliques[in_which_cliques], length)
    idx <- index_in_which_cliques[which.max(length_of_possible_cliques)]
    pot <- x$charge$C[[idx]]
    rm_var <- setdiff(attr(pot, "vars"), z)

    return(marginalize(pot, rm_var))
  })

  if (type == "joint") {
    return(.query[[1]])
  } else {
    return(structure(.query, names = nodes))
  }
}

#' A print method for junction trees
#'
#' @param x A junction tree object, \code{jt}.
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{jt}}
#' @export
print.jt <- function(x, ...) {
  cls <- paste0("<", paste0(class(x), collapse = ", "), ">")
  direction <- attr(x, "direction")
  flow <- attr(x, "flow")
  nv  <- ncol(x$clique_tree)
  ne  <- sum(x$clique_tree)/2  
  clique_sizes <- .map_int(x$cliques, length)
  max_C <- max(clique_sizes)
  min_C <- min(clique_sizes)
  avg_C <- mean(clique_sizes)

  cat(" A Junction Tree With",
    "\n -------------------------",
    "\n  Flow:", flow,
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$cliques),
    "\n   - max:", max_C,
    "\n   - min:", min_C,
    "\n   - avg:", round(avg_C, 2), 
    paste0("\n  ", cls),
    "\n -------------------------\n"
  )
  
}

#' A plot method for junction trees
#'
#' @param x A junction tree object, \code{jt}.
#' @param ... For S3 compatability. Not used.
#' @seealso \code{\link{jt}}
#' @export
plot.jt <- function(x, ...) {
  direction <- attr(x, "direction")
  y <- if (direction == "collect") {
    list(
      cliques   = x$schedule$collect$cliques,
      tree      = x$schedule$collect$tree,
      type      = "directed"
    )
  } else if (direction == "distribute") {
    list(
      cliques = x$schedule$distribute$cliques,
      tree    = x$schedule$distribute$tree,
      type    = "directed"
    )
  } else {
    list(
      cliques = x$cliques,
      tree    = x$clique_tree,
      type    = "undirected"
    )
  }
  .names <- unlist(lapply(y$cliques, function(z) paste(z, collapse = "\n")))
  dimnames(y$tree) <- list(.names, .names)
  g <- igraph::graph_from_adjacency_matrix(y$tree, y$type)
  graphics::plot(g, ...)
}
