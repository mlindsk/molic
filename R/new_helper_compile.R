## Naming?
# - intialize_potentials
# - new_charge - I'll go with this one!
# - make_charge

## new_charge <- function(jt, g) {
##   conditionals <- if (igraph::is.igraph(g)) parents_igraph(g) else NULL # FIX API NOW!!

##   # class = charge....
## }

# update_charge <- function(x) {}

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
## plot(g)
## g <- g + edge(4, 7)
## g <- g + edge(5, 6)
## plot(g)
## g <- add_vertices(g, 3)
## g <- g + edge(9, 10)
## plot(g)
## g <- set.vertex.attribute(g, "name", value = letters[1:10])
## par <- parents_igraph(g)
## g   <- moralize_igraph(g, par)
## plot(g)
## g <- igraph::as.undirected(g)
## plot(g)
## g <- triangulate_igraph(g)
## plot(g)

## g <- make_tree(2)
## g <- igraph::as.undirected(g)
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
# 7) make a schedule - given by parent / child relations

# Propagation:
# ------------
# 1) Send messages/flows either just to the root or to the root and out again.
#  - CollectEvidence and DistributeEvidence
# 2) let both_directions = TRUE be an argument

# The algorithm must take the following representations as input
# - igraph DAGS
# - gengraphs
# - named lists

## jt   <- new_jt(g)
## jt
## pjt <- prune_jt(jt)
## prune_jt(pjt)


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
