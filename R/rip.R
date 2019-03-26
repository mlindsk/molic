nhood <- function(G, v){
  UseMethod("nhood")
}

mcs <- function(G){
  UseMethod("mcs")
}

perfect_sequence <- function(G, v) {
  UseMethod("perfect_sequence")
}

rip <- function(G, v) {
  UseMethod("rip")
}

is_decomposable <- function(G) igraph::is_chordal(G)$chordal # G: igraph

make_vgraph <- function(G) {
  # G: igraph object
  # Out: Vanilla representation of an igraph
  G <- list(E = attributes(igraph::E(G))$vnames, V = attributes(igraph::V(G))$names)
  structure(G, class = c("vgraph", "list"))
}

make_igraph <- function(G) {
  # G: vgrah object
  e <- unlist(lapply(strsplit(G$E, "|"), function(x) x[-2]))
  igraph::graph_from_edgelist(matrix( e, nc = 2, byrow = TRUE), directed = FALSE)
}

nhood.vgraph <- function(G, a) {
  # Not very efficient - only look at the separators (is that possible?)
  # - make a nhood for decomposable graphs maybe
  edges <- strsplit(G$E, "\\|")
  non_neighbor <- function(e) !identical(e, character(0))
  unique(unlist(Filter(non_neighbor, lapply(edges, function(e) e[-which(e == a)]) )))
  ## gsub(paste0("\\|",a,"|",a,"\\|"), "", G$E[grep(a, G$E)])
}



mcs.vgraph <- function(G, init_node = 1L) {
  # G: vgraph object 
  # - G needs to be decomposable/triangulated
  # - It does NOT give a warning if it is not (it is just not a perfect sq)!
  nodes <- G$V
  n     <- length(nodes)
  # if( class(nodes) != "character" ) stop("The nodes of the graph must be characters")
  nodes <- as.character(nodes)
  # Handling singletons
  if( !(n-1) ) return(nodes) 
  labels          <- table(nodes) - 1L
  used_nodes      <- vector(mode = "character", length = n)
  used_nodes[1]   <- nodes[init_node] # Choose another starting vertex?
  v               <- used_nodes[init_node]
  remaining_nodes <- setdiff(nodes, used_nodes)
  for( k in 2:n ) {
    ne          <- nhood(G,v)
    nodes_in_ne <- which(names(labels) %in% ne)
    labels[nodes_in_ne] <- labels[nodes_in_ne] + 1L
    v               <- names(which.max(labels[remaining_nodes]))
    used_nodes[k]   <- v
    remaining_nodes <- setdiff(nodes, used_nodes)
  }
  used_nodes
}

#' Maximum Cardinality Search.
#'
#' @export
mcs.igraph <- function(G) mcs.vgraph(make_vgraph(G))

perfect_sequence.vgraph <- function(G, z) {
  ## z: mcs object
  # See Graphical Models, Lemma 2.14, by Steffen Lauritzen
  # for the correctness of this function.
  lapply(seq_along(z), function(i) {
    ne_i  <- nhood(G, z[i])
    c( z[i], intersect(ne_i, z[1:(i-1)]) )
  })    
}


#' Perfect Sequence Of Cliques 
#'
#' @export

perfect_sequence.igraph <- function(G, z) perfect_sequence(make_vgraph(G), z)

cliques <- function(x) {
  # x: a perfect_sequence of sets (denoted B_i in Lauritzen)
  C_index <- c()
  for( b in seq_along(x) ) {
    in_others <- vapply(X = x[-b], function(z) all(x[[b]] %in% z), TRUE)
    if( !any(in_others) ) C_index <- c(C_index, b)
  }
  x[C_index]
}

separators <- function(C) {
  # C: Cliques (with RIP ordering)
  # S_j := H_{j-1} \cap C_j, H_{j-1} := \cap_k C_{k-1}, k = 1, 2, ..., j-1
  lapply( seq_along(C), function(j) {
    if( j == 1) {
      return(NULL)
    } else {
      return(intersect(C[[j]], unlist(C[1:(j-1)]) ) )
    }
  })
}

rip.vgraph <- function(G) {
  # See Graphical Models, Lemma 2.13 by Steffen Lauritzen
  # if(!igraph::is_chordal(G)$chordal) stop("Graph is not triangulated")
  z  <- mcs(G)
  # Handling singletons:
  if( !(length(z) - 1L) ) return(list(C = list(z), S = list(NULL)))
  C <- cliques(perfect_sequence(G, z))
  S <- separators(C)
  list(C = C, S = S)
}

#' Running Intersection Property Ordering of The Cliques In a Decomposable Graph
#'
#' @param G igraph object. An undirected graph.
#' 
#' @export

rip.igraph <- function(G) rip(make_vgraph(G))

## library(igraph)
## nods <- c(7, 2, 2, 3, 3, 4, 2, 5, 5, 3)
## nods <- as.character(nods)
## G <- make_graph(nods, directed = FALSE)
## separators(cliques(perfect_sequence(G, mcs(G))))
## plot(G)
## mcs(G)
## rip(G)

## https://github.com/cran/gRbase
## https://rdrr.io/cran/gRbase/man/graph-mcs.html
## library(gRbase)
## gRbase::rip(G)
## gRbase::mcs(G)
