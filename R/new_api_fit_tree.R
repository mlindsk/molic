tree_weights <- function(x, df) UseMethod("tree_weights")

tree_weights.gengraph <- function(x, df) {
  # x: gengraph object
  nodes       <- colnames(df) 
  n           <- length(nodes) 
  pairs       <- utils::combn(nodes, 2,  simplify = FALSE) 
  weights     <- structure(vector(mode = "numeric", length = n * (n - 1) / 2), names = "")
  for( j in 1:n ) x$MEM[[nodes[j]]] <- entropy(df[nodes[j]])
  for (p in seq_along(pairs) ) {
    edge   <- sort_(pairs[[p]])
    ed     <- entropy_difference(edge, character(0), df, x$MEM)
    weights[p]        <- ed$ent
    names(weights)[p] <- edge
    ht <- ed$ht # CHANGE TO MEM LATER!
  }
  return(sort(weights, decreasing = TRUE))
}

## d <- tgp_dat[1:100, 5:15]
## g <- gengraph(d, type = "tree")
## sloop::s3_dispatch(print(g))

# Should take a tree object as the first argument
kruskal <- function(df) {
  x          <- tree_weights(df)
  class(x)   <- c("tre", class(x))
  n          <- ncol(df)
  nodes      <- colnames(df)
  x$G_adj    <- structure(replicate(n, character(0)), names = nodes)
  ## x$G_A      <- Matrix::Matrix(0L, n, n, dimnames = list(nodes, nodes))
  node_pairs <- es_to_vs(names(x$weights))
  number_of_nodes_total <- n
  number_of_nodes_added <- 0L
  for (e in seq_along(x$weights)) {
    if( number_of_nodes_added == number_of_nodes_total - 1 ) return(x)
    node1 <- node_pairs[[e]][1]
    node2 <- node_pairs[[e]][2]
    component1 <- dfs(x$G_adj, node1)
    component2 <- dfs(x$G_adj, node2)
    if( !neq_empt_chr(intersect(component1, component2)) ) {
      x$G_adj[[node1]] <- c(x$G_adj[[node1]], node2) # Use add_edge helper function
      x$G_adj[[node2]] <- c(x$G_adj[[node2]], node1) # Use add_edge helper function
      x$G_A[node1, node2] <- 1L                      # Use add_edge helper function
      x$G_A[node2, node1] <- 1L                      # Use add_edge helper function
      number_of_nodes_added <- number_of_nodes_added + 1L
    }
  }
  return(x)
}

tree_as_fwd <- function(df, t) {
  x    <- rip(t$G_adj)               # should be x$CG <- rip(x$G_adj)$C
  CG   <- x$C                        # should be x$CG
  nC   <- length(CG)
  CG_A <- Matrix::Matrix(0L, nC, nC) # Should already be present in x as NULL
  msi_S  <- vector("list", 0L)
  max_dst <- 0
  max_edge  <- ""
  max_nodes <- 0L
  max_idx   <- 0L
  max_ins   <- c()
  k         <- 1L
  for ( i in 2:nC ) {
    for (j in 1:(i-1) ) {
      Ci   <- CG[[i]]
      Cj   <- CG[[j]]
      Sij  <- intersect(Ci, Cj)
      if ( neq_empt_chr(Sij) ) { # Note: This ONLY work for trees  
        CG_A[i,j] = 1L
        CG_A[j,i] = 1L
        Ci_minus_Sij <- setdiff(Ci, Sij)
        Cj_minus_Sij <- setdiff(Cj, Sij)
        edge         <- sort_( c(Ci_minus_Sij, Cj_minus_Sij))
        ee           <- entropy_difference(edge, Sij, df, t$ht)
        ent_ij       <- ee$ent
        t$ht         <- ee$ht
        if( ent_ij >= max_dst ) {
          max_dst  <- ent_ij
          max_edge <- edge
          max_idx  <- k
          max_ins  <- c(i, j)
        }
        msi_S[[k]] <- list(S = Sij, e = structure(ent_ij, names = edge), C1 = Ci, C2 = Cj)
        k <- k + 1L
      }
    }
  }
  max_lst = list(e = max_edge, idx = max_idx, ins = max_ins)
  msi     = list(S = msi_S, max = max_lst)
  out <- list(G_adj = t$G_adj,
    G_A      = t$G_A,
    CG       = CG,
    CG_A     = CG_A,
    MSI      = msi,
    ht       = t$ht
  )
  class(out) <- setdiff(c("fwd", class(out)), "tree")
  return(out)  
}

#' Fitting a Chow-Liu tree interaction graph
#' 
#' @description This functions uses Kruskals algorithm to fit a tree structure to variables in \code{df}
#' 
#' @param df data.frame
#' @param wrap Logical indicating if the object should be converted to a \code{fwd} object or not (can increase runtime significantly if FALSE)
#' @references \url{https://ieeexplore.ieee.org/abstract/document/1054142}
#' @seealso \code{\link{efs}}, \code{\link{efs_step}}, \code{\link{bws}}, \code{\link{bws_step}},\code{\link{adj_list.efs}}, \code{\link{adj_matrix.efs}}
#' @examples
#' G <- cl_tree(tgp_dat[, 5:8])
#' print(G)
#' plot(G)
#' @export
fit_tree <- function(df, wrap = TRUE) {
  if( wrap ) return( tree_as_fwd(df, kruskal(df)) )
  else return( kruskal(df) )
}
