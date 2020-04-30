parents_igraph <- function(g) {
  stopifnot(igraph::is_directed(g), igraph::is_dag(g))
  Ag <- igraph::as_adjacency_matrix(g)
  cn <- colnames(Ag)
  if (is.null(cn)) {
    stop("The vertices in the igraph object must have names")
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

as_undirected_igraph <- function(g) igraph::as.undirected(g)

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

  coll_lvs <- leaves_jt(coll_tree)
  dist_lvs <- leaves_jt(dist_tree)

  attr(coll_tree, "leaves")  <- coll_lvs
  attr(dist_tree, "leaves")  <- dist_lvs

  attr(coll_tree, "parents") <- parents_jt(coll_tree, coll_lvs)
  attr(dist_tree, "parents") <- parents_jt(dist_tree, dist_lvs)

  collect    <- list(cliques = cliques, tree = coll_tree)
  distribute <- list(cliques = cliques, tree = dist_tree)

  return(list(collect = collect , distribute = distribute, clique_tree = clique_tree))
  
}

leaves_jt <- function(x) {
  # x:   rooted tree structure of a junctions tree (jt$schedule$collect$tree)
  which(colSums(x) == 0L)
}

parents_jt <- function(x, lvs) {
  # x:   rooted tree structure of a junctions tree (jt$schedule$collect$tree)
  # lvs: leaves of the junction tree
  par <- vector("list", length = length(lvs))
  for (i in seq_along(lvs)) {
    pari <- which(x[lvs[i], ] == 1L)
    par[[i]] <- pari
  }
  return(par)
}

prune_jt <- function(jt) {

  direction <- attr(jt, "direction")
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute

  if (identical(x, "FULL")) {
    stop("The junction tree has already been propagated in this direction!")  
  }

  leaves <- attr(x$tree, "leaves")
  pars   <- attr(x$tree, "parents")

  
  if (length(leaves) == ncol(x$tree)) { # If all nodes left are singletons in distribute
    x$cliques <- NULL
  } else {
    x$cliques <- x$cliques[-leaves]
    x$tree    <- x$tree[-leaves, -leaves]   
  }
  
  has_arrived_at_root <- length(x$cliques) < 2L
  if (has_arrived_at_root) {
    if (direction == "collect") {
      jt$schedule$collect    <- "FULL"
      attr(jt, "direction")  <- "distribute"
      jt$charge$C[["C1"]] <- jt$charge$C[["C1"]] / sum(jt$charge$C[["C1"]])
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
  } else {
    jt$schedule$distribute <- list(cliques = x$cliques, tree = x$tree)
  }
  
  return(jt)
}

set_evidence_jt <- function(charge, cliques, evidence) {
  for (k in rev(seq_along(cliques))) {
    Ck <- cliques[[k]]
    for (i in seq_along(evidence)) {
      e     <- evidence[i]
      e_var <- names(e)
      e_val <- unname(e)
      if (e_var %in% Ck) {
        e_pos_charge_k    <- match(e_var, attr(charge$C[[k]], "vars"))
        charge_k_by_e_pos <- find_cond_configs(charge$C[[k]], e_pos_charge_k)
        idx_to_keep   <- which(charge_k_by_e_pos == e_val)
        charge$C[[k]] <- charge$C[[k]][idx_to_keep]
      }
    }
  }
  return(charge)
}


new_jt <- function(g, data, evidence = NULL, flow = sum, validate = TRUE) {

  if (validate) {
    if( !only_single_chars(data)) {
      stop("All values in data must be represented as a single character. Use to_single_chars(data)")
    }
    
  }
  
  par_igraph <- NULL

  adj <- if (igraph::is.igraph(g)) {
    par_igraph <- parents_igraph(g)
    g <- moralize_igraph(g, par_igraph)
    g <- igraph::as.undirected(g)
    g <- triangulate_igraph(g)
    as_adj_lst(igraph::as_adjacency_matrix(g))    
  } else if (inherits(g, "gengraph")) {
    adj_lst(g)
  } else {
    stopifnot(is_decomposable(g))
    g
  }

  if (!all(names(adj) %in% colnames(data))) {
    stop("Variable names of the graph does not correspond to variables in data")
  }
  
  rip_    <- rip(adj, check = FALSE)
  cliques <- rip_$C
  names(cliques) <- paste("C", 1:length(cliques), sep = "")

  if (length(cliques) < 2) {
    stop("No need to propagate for |C| < 2... But fix anyway...")
  }

  ## TODO: Specify a root in advance
  ## ---------------------------------------------------------
  # if (is.null(root)) re_order_cliques(cliques, root) { using kruskal?}
  ## See SOREN and Lau p. 58 for specifying another root easily!
  ## ---------------------------------------------------------

  par <- if (!is.null(par_igraph)) par_igraph  else rip_$P

  charge <- new_charge(data, cliques, par)

  if (!is.null(evidence)) charge <- set_evidence_jt(charge, cliques, evidence)
  
  schedule  <- new_schedule(cliques)
  jt        <- list(
    schedule    = schedule[1:2],
    charge      = charge,
    cliques     = cliques,
    clique_tree = schedule$clique_tree)
  class(jt) <- c("jt", class(jt))
  attr(jt, "direction") <- "collect" # collect, distribute or full
  return(jt)
}

send_messages <- function(jt, flow = sum) {
  
  direction <- attr(jt, "direction")
  x   <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute
  lvs <- attr(x$tree, "leaves")
  par <- attr(x$tree, "parents")

  for (k in seq_along(lvs)) {

    lvs_k <- lvs[k]
    par_k <- par[[k]]

    # Skip if the leave has no parents (can occur in distribute)    
    if (!neq_empt_int(par_k)) next
    
    for (pk in par_k) {
      
      C_lvs_k <- x$cliques[[lvs_k]]
      C_par_k <- x$cliques[[pk]]
      Sk      <- intersect(C_lvs_k, C_par_k)

      if (neq_empt_chr(Sk)) { # if empty, no messages should be sent

        C_lvs_k_name <- names(x$cliques)[lvs_k]
        C_par_k_name <- names(x$cliques)[pk]

        message_k_conditional_names <- setdiff(C_lvs_k, Sk)
        
        message_k <- marginalize(jt$charge$C[[C_lvs_k_name]], message_k_conditional_names, flow)

        jt$charge$C[[C_par_k_name]] <- merge(jt$charge$C[[C_par_k_name]], message_k, "*")

        if (direction == "collect") {
          jt$charge$C[[C_lvs_k_name]] <- merge(jt$charge$C[[C_lvs_k_name]], message_k, "/")
          
        }

        if (direction == "distribute") {
          ## TODO: Just paste S and  par_k
          S_k_name <- paste("S", str_rem(C_par_k_name, 1L), sep = "")
          jt$charge$S[[S_k_name]] <- message_k
        }
        
      }      
    }
  }
  prune_jt(jt)
}

## $C1
## [1] "asia" "tub" 

## $C2
## [1] "either" "tub"   

## $C3
## [1] "either" "lung"   "smoke" 

## $C4
## [1] "bronc"  "either" "smoke" 

## $C5
## [1] "bronc"  "dysp"   "either"

## $C6
## [1] "either" "xray"

## NO CLIQUE WITH c("either", "tub", "lung") ? 

new_charge <- function(data, cliques, conditional_parents) {

  potC     <- vector("list", length(cliques))
  potS     <- vector("list", length(cliques))
  children <- names(conditional_parents)

  for (x in children) {
    parx <- conditional_parents[[x]]
    spt  <- sptable(as.matrix(data[, c(x, parx), drop = FALSE]))
    pspt <- parray(spt, parx)
    
    for (k in seq_along(cliques)) {
      family_in_Ck <- all(c(x, parx) %in% cliques[[k]])
      if (family_in_Ck) {
        if (is.null(potC[[k]])) {
          potC[[k]] <- pspt
        } else {
          potC[[k]] <- merge(potC[[k]], pspt)
        }
        break # Must only live in one clique
      }
    }
  }

  # Some clique potentials may be empty due to triangulation
  # We set these to the identity = 1 for all configurations
  which_is_null <- .map_lgl(potC, is.null)

  if (any(which_is_null)) {
    for (k in which(which_is_null)) {
      sptk <- sptable(as.matrix(data[, cliques[[k]], drop = FALSE]))
      sptk[1:length(sptk)] <- 1L
      potC[[k]] <- sptk
    }
  }
  
  names(potS) <- paste("S", 1:length(potS), sep = "")
  names(potC) <- names(cliques)
  pots <- list(C = potC, S = potS)
  return(pots)
}

print.jt <- function(x, ...) {
  nq <- length(x$cliques)
  dir_ <- attr(x, "direction")
  cat("Direction: ", dir_, "\n", " (Fix this print method)\n")
}

plot.jt <- function(x, ...) {
  direction <- attr(x, "direction")
  jt <- if (direction == "collect") x$schedule$collect else x$schedule$distribute
  .names <- unlist(lapply(jt$cliques, function(y) paste(y, collapse = "\n")))
  dimnames(jt$tree) <- list(.names, .names)
  g <- igraph::graph_from_adjacency_matrix(jt$tree)
  graphics::plot(g, ...)
}
