## remove_edge_list <- function(adj, e) {
##   # e is a 2-dim vector of chars
##   adj[[e[1]]] <- setdiff(adj[[e[1]]], e[2])
##   adj[[e[2]]] <- setdiff(adj[[e[2]]], e[1])
##   adj
## }

## remove_edge_mat <- function(A, e) {
##   # e is a 2-dim vector of chars
##   del_idx <- match(e, colnames(A))
##   A[del_idx[1], del_idx[2]] <- 0L
##   A[del_idx[2], del_idx[1]] <- 0L
##   A
## }


## #' Construct a bws object
## #' @description A constructor for bws class
## #' @param df data.frame 
## #' @param adj An adjacency list
## #' @param ht An environment (hashtable) containing precomputed entropies
## #' @return A bws object. See \code{bws} for more details about the returning object.
## #' @examples
## #' d   <- tgp_dat[, 5:8]
## #' cg <- make_complete_graph(colnames(d))
## #' bi <- bws_init(d, cg)
## #' bws_step(d, bi)
## #' @seealso \code{\link{bws_step}}, \code{\link{bws}}, \code{\link{make_complete_graph}}
## #' @export
## bws_init <- function(df, adj, ht = new.env(hash = TRUE)) {
##   stopifnot(is.environment(ht))
##   stopifnot( is.list(adj) || names(adj) != length(adj))
##   structure(list(
##     G_adj = adj,
##     G_A   = as_adj_mat(adj),
##     e     = NULL,            # Best edge to delete
##     C     = rip(adj)$C,      # Clique list,
##     lv    = vapply(df, function(x) length(unique(x)), 1L),
##     ht    = ht),
##     class = c("bws", "list")
##   )
## }


## #' Stepwise backward selection in decomposable graphical models
## #' @description Stepwise backward selection in decomposable graphical models
## #' @param df data.frame
## #' @param x A bws object. Can be created explicitly with \code{bws_init}
## #' @param p Penalty term in the stopping criterion  (\code{0} = AIC and \code{1} = BIC)
## #' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
## #' @return A bws object. See \code{bws} for details about the returning object.
## #' @details See \code{\link{efs}} for details about \code{thres}.
## #' @examples
## #' d  <- tgp_dat[, 5:8]
## #' cg <- make_complete_graph(colnames(d))
## #' bi <- bws_init(d, cg)
## #' bws_step(d, bi)
## #' @seealso \code{\link{bws}}, \code{\link{make_complete_graph}}, \code{\link{efs}}, \code{\link{efs_step}}, \code{\link{cl_tree}}
## #' @export
## bws_step <- function(df, x, p = 0.5, thres = 5) {
##   # x : bws class
##   nodes   <- names(x$G_adj)
##   M       <- nrow(df)
##   e_min   <- Inf
##   for( i in 1:ncol(x$G_A) ) {
##     for( j in 1:i ) {
##       if( x$G_A[i, j] == 1L) {
##         pair <- c(nodes[i], nodes[j])
##         pair_clique_idx <- 1L
##         pair_in_cliques <- sapply(seq_along(x$C), function(z) {
##           pair_in_z <- all(pair %in% x$C[[z]])
##           if ( pair_in_z ) pair_clique_idx <<- z
##           pair_in_z
##         })
##         if( sum(pair_in_cliques) < 2 ) { # Eligeble for deletion
##           C    <- x$C[[pair_clique_idx]]
##           va   <- pair[1]
##           vb   <- pair[2]
##           Ca   <- setdiff(C, vb)
##           Cb   <- setdiff(C, va)
##           S    <- intersect(Ca, Cb)
##           es   <- sort_(pair)
##           ed   <- entropy_difference(es, S, df, x$ht, thres)
##           x$ht <- ed$ht
##           penalty     <- log(M)*p + (1 - p)*2
##           vs          <- pair
##           HM_HM_prime <- ed$ent
##           dev         <- 2*M*HM_HM_prime
##           d_parms     <- -prod(x$lv[pair] - 1) * prod(x$lv[S])
##           d_aic       <- dev + penalty * d_parms
##           if ( d_aic <= e_min ) {
##             e_min <- d_aic
##             x$e  <- structure(c(va, vb), "d_aic" = d_aic)
##             x$S  <- S
##           }
##         }
##       }
##     }  
##   }
##   x$G_adj <- remove_edge_list(x$G_adj, x$e)
##   x$G_A   <- remove_edge_mat(x$G_A, x$e)
##   x$C     <- rip(x$G_adj)$C
##   return(x)
## }


## delta_pic <- function(x, lv, M, p = 0.5) {
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
