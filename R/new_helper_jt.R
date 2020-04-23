parents_igraph <- function(g) {
  stopifnot(igraph::is_directed(g), igraph::is_dag(g))
  Ag <- igraph::as_adjacency_matrix(g)
  cn <- colnames(Ag)
  if (is.null(cn)) { # If no names are given
    .names <- 1:ncol(Ag)
    dimnames(Ag) <- list(.names, .names)
  }
  apply(Ag, 2, function(x) {
    names(which(x == 1L))
  })
}

moralize_igraph <- function(g, parents) {
  if (is.null(igraph::vertex.attributes(g)$name)) { # Remove this in the future - nodes must have meaningful names according to data
    g <- igraph::set_vertex_attr(g, "name", value = 1:igraph::vcount(g))
  }
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

as_undirected_igraph <- function(g) igraph::as.undirected(g)


leaves_jt <- function(x) {
  # x:   rooted tree structure of a junctions tree (jt$schedule$collect$tree)
  which(apply(x, 2L, function(y) sum(y) == 0L))
}

parents_jt <- function(x, lvs) {
  # x:   rooted tree structure of a junctions tree
  # lvs: leaves of the junction tree
  par <- vector("list", length = length(lvs))
  for (i in seq_along(lvs)) {
    pari <- which(x[lvs[i], ] == 1L)
    par[[i]] <- pari
  }
  return(par)
}

## new_singleton_jt <- function(jt, direction) {

##   ## ---------------------------------------------------------
##   ##        FIX: JUST NEED TO CHANGE ONE OF THE TREES!!!!
##   ## ---------------------------------------------------------
##   x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  
##   jt             <- list()
##   coll_tree      <- matrix(0L)
##   dist_tree      <- coll_tree
##   attr(coll_tree, "leaves")   <- integer(0L)
##   attr(coll_tree, "parents")  <- list()
##   attr(dist_tree, "leaves")   <- integer(0L)
##   attr(dist_tree, "parents")  <- list()
##   collect <-  list(cliques = cliques, tree = coll_tree)
##   distribute <-  list(cliques = cliques, tree = dist_tree)
##   attr(collect, "is_singleton")    <- TRUE
##   attr(distribute, "is_singleton") <- TRUE
##   jt$schedule    <- list(collect = collect, distribute = distribute)
##   jt$cliques     <- cliques
##   jt$clique_tree <- matrix(0L)
##   class(jt)   <- c("jt", class(jt))
##   attr(jt, "direction") <- "collect"
##   return(jt)
## }

prune_jt <- function(jt) {
  direction <- attr(jt, "direction")
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute

  if (identical(x, "FULL")) stop("The junction tree has already been propagated in this direction!")

  leaves    <- attr(x$tree, "leaves")
  x$cliques <- x$cliques[-leaves]
  x$tree    <- x$tree[-leaves, -leaves]

  still_not_singleton <- length(x$cliques) > 1L
  if (!still_not_singleton) {
    if (direction == "collect") {
      jt$schedule$collect <- "FULL"
      attr(jt, "direction") <- "distribute"
    } else {
      jt$schedule$distribute <- "FULL"
      attr(jt, "direction")  <- "FULL"
    }
    return(jt)
  }
  
  attr(x$tree, "leaves")  <- leaves_jt(x$tree)
  attr(x$tree, "parents") <- parents_jt(x$tree, attr(x$tree, "leaves"))
  if (direction == "collect") {
    jt$schedule$collect <- list(cliques = x$cliques, tree = x$tree)
    attr(jt$schedule$collect, "is_singleton") <- FALSE
  } else {
    jt$schedule$distribute <- list(cliques = x$cliques, tree = x$tree)
    attr(jt$schedule$distribute, "is_singleton") <- FALSE
  }
  return(jt)
}

new_schedule <- function(cliques) {
  nc <- length(cliques)
  clique_tree <- matrix(0L, nc, nc)
  coll_tree   <- clique_tree
  dist_tree   <- clique_tree

  # Alg. 4.8 - Probabilistic Expert Systems (p.55)
  for (i in seq_along(cliques)[-1L]) {
    for (j in 1:(i-1L)) {
      Ci   <- cliques[[i]]
      Cj   <- cliques[[j]]
      Hi_1 <- unlist(cliques[1:(i-1L)])
      Si   <- intersect(Ci, Hi_1)
      if (all(Si %in% Cj)) {
        clique_tree[i, j]    <- 1L
        clique_tree[j, i]    <- 1L
        is_new_directed_edge <- !neq_empt_int(which(coll_tree[i, ] == 1L))
        if (is_new_directed_edge) {
          coll_tree[i, j] <- 1L
          dist_tree[j, i] <- 1L
        }
      }
    }
  }
  # root  <- which.max(.map_int(cliques, length))
  coll_lvs <- leaves_jt(coll_tree)
  dist_lvs <- leaves_jt(dist_tree)
  attr(coll_tree, "leaves")  <- coll_lvs
  attr(dist_tree, "leaves")  <- dist_lvs
  attr(coll_tree, "parents") <- parents_jt(coll_tree, coll_lvs)
  attr(dist_tree, "parents") <- parents_jt(dist_tree, dist_lvs)
  # names(cliques) <- paste("C", 1:nc, sep = "")
  collect    <- list(cliques = cliques, tree = coll_tree)
  distribute <- list(cliques = cliques, tree = dist_tree)
  attr(collect, "is_singleton")    <- FALSE
  attr(distribute, "is_singleton") <- FALSE
  return(list(collect = collect , distribute = distribute, clique_tree = clique_tree))
  
}

new_jt <- function(g, data, set_evidence = NULL, flow = sum, ...) {

  adj <- if (igraph::is.igraph(g)) {
    as_adj_lst(igraph::as_adjacency_matrix(g))    
  } else if (inherits(g, "gengraph")) {
    adj_lst(g)
  } else {
    stopifnot(is_decomposable(g))
    g
  }
  
  rip_    <- rip(adj, check = FALSE)
  cliques <- rip_$C
  names(cliques) <- paste("C", 1:length(cliques), sep = "")
  if (length(cliques) < 2) {
    stop("Fix this...")
    return(new_singleton_jt(cliques))
  }

  # if (is.null(root)) re_order_cliques(cliques, root) { using kruskal?}
  par       <- if (igraph::is.igraph(g)) parents_igraph(g) else rip_$P
  charge    <- new_charge(data, cliques, par)
  schedule  <- new_schedule(cliques)
  jt        <- list(schedule = schedule[1:2], charge = charge, cliques = cliques, clique_tree = schedule$clique_tree)
  class(jt) <- c("jt", class(jt))
  attr(jt, "direction") <- "collect" # collect, distribute or full
  return(jt)
}

plot_jt <- function(jt, ...) {
  .names <- unlist(lapply(jt$schedule$collect$cliques, function(x) paste(x, collapse = "\n")))
  x <- jt$schedule$collect$tree
  dimnames(x) <- list(.names, .names)
  g <- igraph::graph_from_adjacency_matrix(x)
  plot(g, ...)
}

new_charge <- function(data, cliques, conditional_parents) {
  potC <- vector("list", length(cliques))
  potS <- vector("list", length(cliques))
  children <- names(conditional_parents)
  for (x in children) {
    par  <- conditional_parents[[x]]
    spt  <- sptable(as.matrix(d[, c(x, par), drop = FALSE]))
    pspt <- parray(spt, par)
    for (k in seq_along(cliques)) {
      if (all(c(x, par) %in% cliques[[k]])) {
        if (is.null(potC[[k]])) {
          potC[[k]] <- pspt
        } else {

          ## try_this <- try(merge(potC[[k]], pspt))
          ## if (inherits(try_this, "try-error")) browser()
          
          potC[[k]] <- merge(potC[[k]], pspt)
        }
      }
    }
  }
  names(potS) <- paste("S", 1:length(potS), sep = "")
  names(potC) <- names(cliques)
  pots <- list(C = potC, S = potS)
  return(pots)
}

send_messages <- function(jt) {
  direction <- attr(jt, "direction")
  x   <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  lvs <- attr(x$tree, "leaves")
  par <- attr(x$tree, "parents")

  for (k in seq_along(lvs)) {
    lvs_k <- lvs[k]
    par_k <- par[[k]]
    for (pk in par_k) {
      Ck  <- x$cliques[[lvs_k]]
      Cpk <- x$cliques[[pk]]
      Sk  <- intersect(Ck, Cpk)
      if (neq_empt_chr(Sk)) { # if empty, no messages should be sent
        ## v <- attr(jt$charge$C[[lvs_k]], "vars")
        ## if (any(is.na(match(Sk, v)))) browser()
        ## ---------------------------------------------------------
        ##                         FIX!
        ## ---------------------------------------------------------
        # x$cliques and jt$cliques does not correspond after a single prune!
        # Thus lvs_k is not the correct index for jt$charge$potC... Hmmm
        ## ---------------------------------------------------------
        
        Ck_names  <- names(x$cliques)[lvs_k]
        Cpk_names <- names(x$cliques)[pk]
        
        message_k <- marginalize(jt$charge$C[[Ck_names]], Sk)
        jt$charge$C[[Cpk_names]]    <- merge(jt$charge$C[[Cpk_names]], message_k, "*")
        jt$charge$C[[Ck_names]] <- merge(jt$charge$C[[Ck_names]], message_k, "/")
        ## We need to check for direction here; for distribute we also need to update potS
      }      
    }
  }
  prune_jt(jt)
}

## library(dplyr)
## d    <- tgp_dat[1:500, 5:40]
## g    <- fit_graph(d, trace = FALSE)
## gjt  <- new_jt(g, d)
## pgjt <- prune_jt(gjt)
## plot_jt(gjt)
## plot_jt(pgjt)


## m <- send_messages(gjt)
## m <- send_messages(m)
## plot_jt(m)

## send_messages(sgjt)

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
## new_jt(g)

## h <- make_tree(2)
## p <- parents_igraph(h)
## h <- as_undirected_igraph(triangulate_igraph(moralize_igraph(h, p)))
## new_jt(h)

## jtg <- new_jt(g)
## prune_jt(jtg)
