prune_jt <- function(jt) {

  direction <- attr(jt, "direction")
  x <- if (direction == "collect") jt$schedule$collect else jt$schedule$distribute

  if (identical(x, "FULL")) stop("The junction tree has already been propagated in this direction!")

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


new_jt <- function(g, data, evidence = NULL, flow = sum, ...) {

  par_igraph <- NULL

  adj <- if (igraph::is.igraph(g)) {
    par_igraph <- parents_igraph(g)
    g <- igraph::as.undirected(g)
    g <- triangulate_igraph(g)
    as_adj_lst(igraph::as_adjacency_matrix(g))    
  } else if (inherits(g, "gengraph")) {
    adj_lst(g)
  } else {
    stopifnot(is_decomposable(g))
    g
  }

  if (!all(names(adj) %in% colnames(data))) stop("Variable names of the graph does not correspond to variables in data")
  
  rip_    <- rip(adj, check = FALSE)
  cliques <- rip_$C
  names(cliques) <- paste("C", 1:length(cliques), sep = "")
  if (length(cliques) < 2) {
    stop("No need to propagate for |C| < 2... But fix anyway...")
  }

  # if (is.null(root)) re_order_cliques(cliques, root) { using kruskal?}
  ## See SOREN and Lau p. 58 for specifying another root easily!
  
  par    <- if (!is.null(par_igraph)) par_igraph  else rip_$P
  charge <- new_charge(data, cliques, par)
  
  if (!is.null(evidence)) {
    # TO DO:
    # -----
    # Set the correct entries to zero!
  }
  
  schedule  <- new_schedule(cliques)
  jt        <- list(schedule = schedule[1:2], charge = charge, cliques = cliques, clique_tree = schedule$clique_tree)
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

        message_k_conditional_names <- setdiff(x$cliques[[lvs_k]], Sk)
        message_k                   <- marginalize(jt$charge$C[[C_lvs_k_name]], message_k_conditional_names, flow)
        jt$charge$C[[C_par_k_name]] <- merge(jt$charge$C[[C_par_k_name]], message_k, "*")

        if (direction == "collect") {
          jt$charge$C[[C_lvs_k_name]] <- merge(jt$charge$C[[C_lvs_k_name]], message_k, "/")
          
        }

        if (direction == "distribute") {
          S_k_name <- paste("S", str_rem(C_par_k_name, 1L), sep = "")
          jt$charge$S[[S_k_name]] <- message_k
        }
        
      }      
    }
  }
  prune_jt(jt)
}

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
          break # Must only live in one clique
        } else {
          potC[[k]] <- merge(potC[[k]], pspt)
          break
        }
      }
    }
  }
  
  names(potS) <- paste("S", 1:length(potS), sep = "")
  names(potC) <- names(cliques)
  pots <- list(C = potC, S = potS)
  return(pots)
}


## ---------------------------------------------------------
##                   EXAMPLE 1
##                 (igraph DAG)
## ---------------------------------------------------------

#### FIX --- SOME CLIQUE POTENTIALS MAY BE SET TO THE IDENTITY
#### SINCE NO CONDITIOANLS CORRESPOND TO THIS DUE TO TRIANGULATION
#### TAKE THIS INTO ACCOUNT!!!

## library(igraph)
## d <- tgp_dat[1:500, 5:70]
## set.seed(7)
## g <- make_tree(7)
## g <- g + edge(4, 7)
## g <- g + edge(5, 6)
## g <- add_vertices(g, 3)
## g <- g + edge(9, 10)
## g <- set.vertex.attribute(g, "name", value = letters[1:10])
## plot(g)

## di <- d %>% select(5:14)
## colnames(di) <- letters[1:10]
## di

## jt <- new_jt(g, di)
## plot_jt(jt)

## m  <- send_messages(jt)
## plot_jt(m)

## m2 <- send_messages(m)
## plot_jt(m2)

## # Sanity check
## sum(m2$charge$C$C1)
## sum(m2$charge$C$C2)

## m3 <- send_messages(m2)
## plot_jt(m3)

## m4 <- send_messages(m3)
## m4

## m4$charge

## sum(m4$charge$C$C3)
## sum(m4$charge$S$S3)

## attr(m4$charge$C[[4]], "vars")

## pa <- parray(sptable(di %>%  select(f, c, e) %>%  as.matrix()))
## pa
## m4$charge$C[[4]]


## ---------------------------------------------------------
##                   EXAMPLE 2
##               (Decomposable MRF)
## ---------------------------------------------------------
## library(dplyr)
## d <- digits[, 5:50] %>% as_tibble()
## g <- fit_graph(d, trace = FALSE, q = 0)
## for (k in 1:50) g <- walk(g, d)

## gjt  <- new_jt(g, d, flow = sum)
## par(mfrow = 1:2)
## plot(g, vertex.size = 2)
## plot_jt(gjt, vertex.size = 15)

## m <- send_messages(gjt)
## while (attr(m, "direction") != "FULL") m <- send_messages(m)

## c1 <- m$charge$C$C1; c1
## v1 <- attr(c1, "vars")
## p1 <- parray(sptable(as.matrix(d[, v1])))[names(c1)]
## round(c1 - p1, 10)

## c2 <- m$charge$C$C2; c2
## v2 <- attr(c2, "vars")
## p2 <- parray(sptable(as.matrix(d[, v2])))[names(c2)]
## round(c2 - p2, 10)

## c3 <- m$charge$C$C3; c3
## v3 <- attr(c3, "vars")
## p3 <- parray(sptable(as.matrix(d[, v3])))[names(c3)]
## round(c3 - p3, 10)

## c4 <- m$charge$C$C4; c4
## v4 <- attr(c4, "vars")
## p4 <- parray(sptable(as.matrix(d[, v4])))[names(c4)]
## round(c4 - p4, 10)

## c5 <- m$charge$C$C5; c5
## v5 <- attr(c5, "vars")
## p5 <- parray(sptable(as.matrix(d[, v5])))[names(c5)]
## round(c5 - p5, 10)

## c40 <- m$charge$C$C40; c40
## v40 <- attr(c40, "vars")
## p40 <- parray(sptable(as.matrix(d[, v40])))[names(c40)]
## round(c40 - p40, 10)

## ---------------------------------------------------------
##                   EXAMPLE 3
##               (Decomposable MRF)
## ---------------------------------------------------------
## library(dplyr)
## d <- tgp_dat[1:500, 5:70] # with 200 additional edges.  Rounding errors.
## d <- tgp_dat[1:500, 5:50] # Rounding errors - but close! (38 cliques)
## d <- tgp_dat[1:500, 5:30] # Rounding errors - way off! (20 cliques)
## d <- tgp_dat[1:500, 5:20] # Relatively close!

## d <- tgp_dat[1:500, 5:10] # Exact!

## d <- tgp_dat[1:500, 5:11] # Exact!

## d <- tgp_dat[, 5:15] # Off by a lot!

## d <- d %>%
##   to_single_chars() %>%
##   as.data.frame() %>%
##   as_tibble() %>%
##   mutate_all(as.character)

## colnames(d) <- c(letters, LETTERS, 1:9)[1:ncol(d)]

## g   <- fit_graph(d, trace = FALSE)
## gjt <- new_jt(g, d)


## par(mfrow = c(1, 2))
## plot.gengraph(g, structure(rep("orange", ncol(d)), names = colnames(d)), vertex.size = 10)
## plot_jt(gjt, vertex.size = 15)


## m <- send_messages(gjt)
## ## # while (m$schedule$collect != "FULL") m <- send_messages(m)
## while (attr(m, "direction") != "FULL") m <- send_messages(m)

## c1 <- m$charge$C$C1; c1
## v1 <- attr(c1, "vars")
## parray(sptable(as.matrix(d[, v1])))[names(c1)]

## c2 <- m$charge$C$C2; c2
## v2 <- attr(c2, "vars")
## parray(sptable(as.matrix(d[, v2])))[names(c2)]

## c3 <- m$charge$C$C3; c3
## v3 <- attr(c3, "vars")
## parray(sptable(as.matrix(d[, v3])))[names(c3)]

## c4 <- m$charge$C$C4; c4
## v4 <- attr(c4, "vars")
## parray(sptable(as.matrix(d[, v4])))[names(c4)]

## c5 <- m$charge$C$C5; c5
## v5 <- attr(c5, "vars")
## parray(sptable(as.matrix(d[, v5])))[names(c5)]
