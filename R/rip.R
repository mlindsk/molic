nhood <- function(A, a) dimnames(A)[[1]][as.logical(A[, a])]

mcs <- function(A, init_node = 1L) {
  # A: Neighbormatrix of a decomposable/triangulated graph G
  # - It does NOT give a warning if it is not (it is just not a perfect sq)!
  nodes <- dimnames(A)[[1]]
  n     <- length(nodes)
  # if( class(nodes) != "character" ) stop("The nodes of the graph must be characters")
  nodes <- as.character(nodes)
  # Handling singletons
  if( !(n-1) ) return(nodes) 
  labels          <- table(nodes) - 1L
  used_nodes      <- vector(mode = "character", length = n)
  used_nodes[1]   <- nodes[init_node] # Choose another starting vertex?
  v               <- used_nodes[init_node]
  remaining_nodes <- nodes[-(nodes == v)]
  for( k in 2:n ) {
    ne          <- nhood(A,v)
    nodes_in_ne <- which(names(labels) %in% ne)
    labels[nodes_in_ne] <- labels[nodes_in_ne] + 1L
    v               <- names(which.max(labels[remaining_nodes]))
    used_nodes[k]   <- v
    remaining_nodes <- remaining_nodes[-(remaining_nodes == v)] 
  }
  used_nodes
}

perfect_sequence <- function(A, z) {
  ## z: mcs object
  # See Graphical Models, Lemma 2.14, by Steffen Lauritzen
  # for the correctness of this function.
  lapply(seq_along(z), function(i) {
    ne_i  <- nhood(A, z[i])
    c( z[i], intersect(ne_i, z[1:(i-1)]) )
  })    
}

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

rip <- function(A) {
  # See Graphical Models, Lemma 2.13 by Steffen Lauritzen
  # if(!igraph::is_chordal(G)$chordal) stop("Graph is not triangulated")
  z  <- mcs(A)
  # Handling singletons:
  if( !(length(z) - 1L) ) return(list(C = list(z), S = list(NULL)))
  C <- cliques(perfect_sequence(A, z))
  S <- separators(C)
  list(C = C, S = S)
}


## library(igraph)
## N <- 1000
## q <- vector(length = 0L)
## for(i in 2:N) q <- c(q, rep(i, 2))
## nods <- as.character(q[1:(length(q)-3)])
## nods <- c("1", nods)
## ## nods <- c(7, 2, 2, 3, 3, 4, 2, 5, 5, 3)
## ## nods <- as.character(nods)
## G <- make_graph(nods, directed = FALSE)
## plot(G)
## ## separators(cliques(perfect_sequence(G, mcs(G))))
## A <- igraph::as_adjacency_matrix(G)
## mcs(A)
## rip(A)

## ## https://github.com/cran/gRbase
## ## https://rdrr.io/cran/gRbase/man/graph-mcs.html
## library(gRbase)
## gRbase::rip(G)
## gRbase::mcs(G)

## library(microbenchmark)
## microbenchmark(gRbase::rip(G), rip(A), times = 2)
