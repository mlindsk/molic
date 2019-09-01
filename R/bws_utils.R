remove_edge_list <- function(adj, e) {
  # e is a 2-dim vector of chars
  adj[[e[1]]] <- setdiff(adj[[e[1]]], e[2])
  adj[[e[2]]] <- setdiff(adj[[e[2]]], e[1])
  adj
}

remove_edge_mat <- function(A, e) {
  # e is a 2-dim vector of chars
  del_idx <- match(e, colnames(A))
  A[del_idx[1], del_idx[2]] <- 0L
  A[del_idx[2], del_idx[1]] <- 0L
  A
}

#' Initialize a bws object
#' @description Initialize a bws object
#' @param adj An adjacency list
#' @param ht An environment (hashtable) containing precomputed entropies
#' @return A bws object. See \code{bws} for more details about the returning object.
#' @examples
#' d <- tgp_dat[, 5:8]
#' bws_init(make_complete_graph(colnames(d)))
#' @seealso \code{\link{bws_step}}, \code{\link{bws}}, \code{\link{make_complete_graph}}
#' @export
bws_init <- function(adj, ht = new.env(hash = TRUE)) {
  stopifnot(is.environment(ht))
  stopifnot( is.list(adj) || names(adj) != length(adj))
  structure(list(
    G_adj = adj,
    G_A   = as_adj_mat(adj),
    e     = NULL,
    S     = character(0),
    C     = list(), # Clique list,
    lv    = sapply(df, function(l) length(unique(l))),
    ht    = ht),
    class = "bws"
  )
}

#' Stepwise backward selection in decomposable graphical models
#' @description Stepwise backward selection in decomposable graphical models
#' @param df data.frame
#' @param x A bws object. Can be initialized with \code{bws_init}
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @return A bws object. See \code{bws} for details about the returning object.
#' @details See \code{\link{efs}} for details about \code{thres}.
#' @examples
#' d <- tgp_dat[, 5:8]
#' bws_step(d, bws_init(make_complete_graph(colnames(d))))
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100}
#' @seealso \code{\link{bws}}, \code{\link{make_complete_graph}}, \code{\link{efs}}, \code{\link{efs_step}}, \code{\link{cl_tree}}
#' @export
bws_step <- function(df, x, p = 0.5, thres = 5) {
  # x : bws class
  adj     <- x$G_adj
  ht      <- x$ht
  cliques <- rip(adj)$C # Make a rip function that does not test for decomposability
  A       <- x$G_A
  nodes   <- names(adj)
  e_min   <- Inf
  e_del   <- vector("character", 2L)
  S_e     <- character(0)
  for( i in 1:ncol(A) ) {
    for( j in 1:i ) {
      if( A[i, j] == 1L) {
        pair <- c(nodes[i], nodes[j])
        pair_clique_idx <- 1L
        pair_in_cliques <- sapply(seq_along(cliques), function(z) {
          pair_in_z <- all(pair %in% cliques[[z]])
          if ( pair_in_z ) pair_clique_idx <<- z
          pair_in_z
        })
        if( sum(pair_in_cliques) < 2 ) { # Eligeble for deletion
          C   <- cliques[[pair_clique_idx]]
          va  <- pair[1]
          vb  <- pair[2]
          Ca  <- setdiff(C, vb)
          Cb  <- setdiff(C, va)
          S   <- intersect(Ca, Cb)
          es  <- sort_(pair)
          ee  <- edge_entropy(es, S, df, ht, thres)
          M           <- nrow(df)
          penalty     <- log(M)*p + (1 - p)*2
          vs          <- pair
          HM_HM_prime <- ee$ent
          dev         <- 2*M*HM_HM_prime
          d_parms     <- -prod(x$lv[pair] - 1) * prod(x$lv[S])
          ## d_aic       <- dev + penalty * d_parms
          ent <- dev + penalty * d_parms # ee$ent
          ht  <- ee$ht
          if ( ent <= e_min ) {
            e_min <- ent
            e_del <- c(va, vb)
            S_e   <- S
          }
        }
      }
    }  
  }
  adj     <- remove_edge_list(adj, e_del)
  A       <- remove_edge_mat(A, e_del)
  attr(e_del, "ent") <- e_min
  structure(list(
    G_adj = adj,
    G_A   = A,
    e     = e_del,
    S     = S_e,
    C     = cliques,
    lv    = x$lv,
    ht    = ht),
    class = "bws"
  )
}

## delta_xic.bws <- function(x, lv, M, p = 0.5) {
##   # x : bws object
##   penalty     <- log(M)*p + (1 - p)*2
##   S           <- x$S
##   vs          <- x$e
##   HM_HM_prime <- attr(x$e, "ent")
##   dev         <- 2*M*HM_HM_prime
##   d_parms     <- -prod(lv[vs] - 1) * prod(lv[S])
##   d_aic       <- dev + penalty * d_parms
##   return(d_aic)
## }
