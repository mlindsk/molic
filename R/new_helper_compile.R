parents <- function(g) UseMethod("parents")

parents.igraph <- function(Ag) {
  stopifnot(igraph::is_directed(g), igraph::is_dag(g))
  Ag <- igraph::as_adjacency_matrix(g)
  cn <- colnames(Ag)
  if (is.null(cn)) {
    .names <- 1:ncol(Ag)
    dimnames(Ag) <- list(.names, .names)
  }
  apply(Ag, 2, function(x) {
  names(which(x == 1L))
  })
}

moralize <- function(g, parents) UseMethod("moralize")

moralize.igraph <- function(g, parents) {
  for (p in parents) {
    if (length(p) > 1) {
      pairs <- utils::combn(p, 2,  simplify = FALSE)
      for (ps in pairs) {
        g <- g + igraph::edge(ps[1], ps[2])
      }
    }
  }
  return(g)
}

triangulate <- function(g) UseMethod("triangulate")

triangulate.igraph <- function(g) {
  igraph::is_chordal(g, fillin = FALSE, newgraph = TRUE)$newgraph
}


## set.seed(7)
## g <- make_tree(15)
## g <- set.vertex.attribute(g, "name", value = letters[1:15])
## plot(g)
## g <- g + edge("m","j")
## g <- g + edge("o","i")
## g <- g + edge("k","h")
## g <- g + edge("l","n")
## plot(g)


## library(igraph)
## set.seed(7)
## g <- make_tree(7)
## g <- set.vertex.attribute(g, "name", value = letters[1:7])
## plot(g)
## g <- g + edge("d","g")
## g <- g + edge("e","f")
## plot(g)

## par <- parents(g)
## g   <- moralize(g, par)
## plot(g)

## g <- igraph::as.undirected(g)
## plot(g)
## g <- triangulate(g)
## plot(g)

## Ag  <- igraph::as_adjacency_matrix(g)
## adj <- as_adj_lst(Ag)


# The conditional densities are now given as:
## par      # igraph DAG
## rip(adj) # decomposable MRF


# Compile:
# --------
# 1) moralize
# 2) triangulate
# 3) initialize_potentials = populate potentials and construct a charge
#    - charge is just the collection of the potentials
# 4) construct the junction tree
# 5) maybe set evidence
# 6) let root be an argument
# 7) make a schedule

# Propagation:
# ------------
# 1) Send messages/flows either just to the root or to the root and out again.
#  - CollectEvidence and DistributeEvidence
# 2) let both_directions = TRUE be an argument

# The algorithm must take the following representations as input
# - igraph DAGS
# - gengraphs
# - named lists


# TODO:
# Find the cliques and make the junction tree. It must be an object
# consisting of a list with the cliques and an non-symmetric matric
# specifying the directions
