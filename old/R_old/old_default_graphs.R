efs_graph <- function(df, component_list) {
  lapply(component_list, function(x) {
    efs(df[, x], trace = FALSE)$G
  })
}

mst_graph <- function(df, component_list) {
  lapply(component_list, function(x) {
    df_x <- df[, unlist(x)]
    A    <- weight_matrix(df_x)
    G    <- igraph::graph.adjacency(A, mode = "undirected", weighted = TRUE)
    igraph::as.undirected(igraph::mst(G))
  })
}

sat_graph <- function(component_list) {
  lapply(component_list, function(x) {
    Cx <- igraph::make_full_graph(length(x))
    igraph::V(Cx)$name <- x
    Cx
  })
}

ind_graph <- function(component_list) {
  lapply(component_list, function(x) {
    C1 <- igraph::graph.empty(n=1, directed=FALSE)
    igraph::V(C1)$name <- x
    C1
  })
}
