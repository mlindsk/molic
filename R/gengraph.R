gengraph <- function(df, adj) {
  if( !setequal(colnames(df), names(adj)) ) stop("column names of df does not correspond to adj")
  structure(list(
    G_adj = adj,
    G_A   = as_adj_mat(adj),
    CG    = NULL,
    CG_A  = NULL, # only used by forward
    MSI   = NULL, # only used by forward
    e     = NULL, # only used by backward
    LV    = vapply(df, function(x) length(unique(x)), 1L),
    MEM   = new.env(hash = TRUE)
  ),
  class = c("gengraph", "list")
  )
}

fwd_select_step <- function(x, df, p = 0.5, thres = 5) UseMethod("fwd_select_step")
bwd_select_step <- function(x, df, p = 0.5, thres = 5) UseMethod("bwd_select_step")

bwd_select_step.gengraph <- function(x, df, p = 0.5, thres = 5,...) {
  class(x) <- c("bwd", class(x))
  x$CG    <- rip(x$G_adj)$C
  nodes   <- names(x$G_adj)
  M       <- nrow(df)
  e_min   <- Inf
  for( i in 1:ncol(x$G_A) ) {
    for( j in 1:i ) {
      if( x$G_A[i, j] == 1L) {
        pair <- c(nodes[i], nodes[j])
        pair_clique_idx <- 1L
        pair_in_cliques <- sapply(seq_along(x$CG), function(z) {
          pair_in_z <- all(pair %in% x$CG[[z]])
          if ( pair_in_z ) pair_clique_idx <<- z
          pair_in_z
        })
        if( sum(pair_in_cliques) < 2 ) { # Eligeble for deletion
          C     <- x$CG[[pair_clique_idx]]
          va    <- pair[1]
          vb    <- pair[2]
          Ca    <- setdiff(C, vb)
          Cb    <- setdiff(C, va)
          S     <- intersect(Ca, Cb)
          es    <- sort_(pair)
          ed    <- entropy_difference(es, S, df, x$MEM, thres)
          x$MEM <- ed$ht   ## FIX TO MEM LATER - BUT KEEP FOR COMPATABILITY NOW! ##
          penalty     <- log(M)*p + (1 - p)*2
          vs          <- pair
          HM_HM_prime <- ed$ent
          dev         <- 2*M*HM_HM_prime
          d_parms     <- -prod(x$lv[pair] - 1) * prod(x$lv[S])
          d_aic       <- dev + penalty * d_parms
          if ( d_aic <= e_min ) {
            e_min <- d_aic
            x$e  <- structure(c(va, vb), "d_aic" = d_aic)
            x$S  <- S
          }
        }
      }
    }  
  }
  x$G_adj <- remove_edge_list(x$G_adj, x$e)
  x$G_A   <- remove_edge_mat(x$G_A, x$e)
  x$CG    <- rip(x$G_adj)$C
  return(x)
}


d  <- tgp_dat[, 3:30]
nodes <- colnames(d)
cg <- make_complete_graph(colnames(d))
G  <- gengraph(d, cg)
bwd_select_step(G, d)
