## --------------------------------------------------
## CONVERT ALL OF THESE TO ADJ-LISTS
## --------------------------------------------------

#' efs_graph
#'
#' Learns a decomposable graph from data
#' 
#' @param df dataframe
#' @param component_list list of components
#'
#' @return A decomposable graph
#' @export
efs_graph <- function(df, component_list) {
  lapply(component_list, function(x) {
    efs(df[, x], trace = FALSE)$G
  })
}

#' @return A tree graph
#' @export
mst_graph <- function(df, component_list) {
  lapply(component_list, function(x) {
    df_x <- df[, unlist(x)]
    A    <- weight_matrix(df_x)
    G    <- igraph::graph.adjacency(A, mode = "undirected", weighted = TRUE)
    igraph::as.undirected(igraph::mst(G))
  })
}

#' @return A complete graph
#' @export
sat_graph <- function(component_list) {
  lapply(component_list, function(x) {
    Cx <- igraph::make_full_graph(length(x))
    igraph::V(Cx)$name <- x
    Cx
  })
}

#' @return A graph with empty edge set
#' @export
ind_graph <- function(component_list) {
  lapply(component_list, function(x) {
    C1 <- igraph::graph.empty(n=1, directed=FALSE)
    igraph::V(C1)$name <- x
    C1
  })
}
