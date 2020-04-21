parents_igraph <- function(Ag) {
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


moralize_igraph <- function(g, parents) {
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

triangulate_igraph <- function(g) {
  igraph::is_chordal(g, fillin = FALSE, newgraph = TRUE)$newgraph
}

leaves_jt <- function(rt) which(apply(rt, 2L, function(x) sum(x) == 0L))

parents_jt <- function(rt, lvs) {
  # rt:  rooted tree structure of a junctions tree
  # lvs: leaves of the junction tree
  par <- vector("integer", length = length(lvs))
  for (i in seq_along(lvs)) {
    pari <- which(rt[lvs[i], ] == 1L)
    par[i] <- pari
  }
  return(par)
}

prune_jt <- function(jt) {
  # remove leaves of cliques, tree and rtree
  # now update the leaves and the parents
  lvs      <- jt$leaves
  jt$C     <- jt$C[-lvs]
  if (length(jt$C) == 1L) {
    jt$tree  <- matrix(0L)
    jt$rtree <- matrix(0L)
    jt$leaves  <- numeric(0L)
    jt$parents <- numeric(0L)
    attr(jt, "is_singleton") <- TRUE
    return(jt)
  }
  jt$tree  <- jt$tree[-lvs, -lvs]
  jt$rtree <- jt$rtree[-lvs, -lvs]
  jt$leaves  <- leaves_jt(jt$rtree)
  jt$parents <- parents_jt(jt$rtree, jt$leaves)
  return(jt)
}

new_jt <- function(g, ...) {

  adj <- if (igraph::is.igraph(g)) {
    Ag   <- igraph::as_adjacency_matrix(g)
    as_adj_lst(Ag)    
  } else if (inherits(g, "gengraph")) {
    g$G_adj
  } else {
    g
  }
  ## CLIQUE 1, MUST BE ROOT - CAN WE CHANGE THAT TO BE USER-SPECIFIC?
  ## OR JUST THE LARGEST CLIQUE ?
  .rip <- rip(adj)
  conditionals <- if (!is.null(conditionals)) .rip$P else conditioanals
  
  cliques <- .rip$C
  nc    <- length(cliques)
  ## In this case we need to be more defensive with the leaves etc: To be continued....
  if (nc < 2) stop("Not implementet for complete graphs yet")
  tree  <- matrix(0L, nc, nc)
  rtree <- matrix(0L, nc, nc)
  # Alg. 4.8 - Probabilistic Expert Systems (p.55)
  for (i in seq_along(cliques)[-1L]) {
    for (j in 1:(i-1L)) {
      Ci   <- cliques[[i]]
      Cj   <- cliques[[j]]
      Hi_1 <- unlist(cliques[1:(i-1L)])
      Si   <- intersect(Ci, Hi_1)
      if (all(Si %in% Cj)) {
        tree[i, j] <- 1L
        tree[j, i] <- 1L
        if (!neq_empt_int(which(rtree[i, ] == 1L))) {
          rtree[i, j] <- 1L
        }
      }
    }
  }
  # root  <- which.max(.map_int(cliques, length))
  lvs <- leaves_jt(rtree)
  par <- parents_jt(rtree, lvs)
  jt  <- list(C = cliques, rtree = rtree, tree = tree, leaves = lvs, parents = par)
  class(jt) <- c("jt", class(jt))
  attr(jt, "is_singleton") <- FALSE
  return(jt)
}

make_charge <- function(jt, g) {
  conditionals <- if (igraph::is.igraph(g)) parents_igraph(g) else NULL # FIX API NOW!!
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


library(igraph)
set.seed(7)
g <- make_tree(7)
plot(g)
g <- g + edge(4, 7)
g <- g + edge(5, 6)
plot(g)
g <- add_vertices(g, 3)
g <- g + edge(9, 10)
plot(g)
g <- set.vertex.attribute(g, "name", value = letters[1:10])
par <- parents_igraph(g)
g   <- moralize_igraph(g, par)
plot(g)
g <- igraph::as.undirected(g)
plot(g)
g <- triangulate_igraph(g)
plot(g)

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


jt   <- new_jt(g)
jt
pjt <- prune_jt(jt)
prune_jt(pjt)


## propagate <- function(jt) {

##   browser()
##   tmp_C       <- jt$C
##   tmp_rtree   <- jt$rtree
##   tmp_leaves  <- leaves(tmp_rtree)
##   tmp_parents <- parents_jt(tmp_rtree,  tmp_leaves)

##   # Push the first message here?
  
##   while (neq_empt_int(tmp_leaves)) { # use attr(jt, "is_singleton") instead!
##     tmp_C    <- tmp_C[-tmp_leaves]
##     tmp_rtree <- tmp_tree[-tmp_leaves, -tmp_leaves]
##     tmp_leaves <- leaves(tmp_rtree)
##     tmp_parents <- parents_jt(jt, tmp_leaves)

##     # Pass the following messages here!

##   }
## }

## new_charge <- function(data, jt) {
  
## }
