# Give more arguments so we can call these constructors in the internal functions
new_gengraph <- function(df, adj, ...) {
  if( !setequal(colnames(df), names(adj)) ) stop("column names of df does not correspond to adj")
  structure(list(
    G_adj = adj,                                            # Graph as adjacency list
    G_A   = as_adj_mat(adj),                                # Graph as adjacency matrix
    CG    = NULL,                                           # Clique list
    LV    = vapply(df, function(x) length(unique(x)), 1L),  # Level vector (for stopping criteria)
    MEM   = new.env(hash = TRUE)),                          # Memoiser - saving entropies to reuse
    class = c("gengraph", "list")
  )
}

new_bwd <- function(df, adj = NULL, q = 0.5) {
  if(is.null(adj)) adj <- make_complete_graph(colnames(df))
  g    <- new_gengraph(df, adj)
  g$CG <- rip(adj)$C  # Use a rip version without check for decomposability
  g$e  <- NULL        # The newly deleted edge
  structure(g, class = c("bwd", class(g)))
}

new_fwd <- function(df, adj = NULL, q = 0.5) {
  is_graph_null <- is.null(adj)
  if( is_graph_null ) adj <- make_null_graph(colnames(df))
  g      <- new_gengraph(df, adj)
  if( is_graph_null ) {
    g$CG_A <- as_adj_mat(make_complete_graph(colnames(df))) # Can be more efficient!
    g$CG   <- as.list(names(adj))
  }
  else {
    stop("Not implementet for general graphs yet.") # Fix this to handle the conversion
  } 
  g$MSI <- list(S = NULL, max = list(e = character(0), idx = numeric(0), ins = vector("numeric", 2L)))
  g$e   <- new_edge()
  g     <- fwd_init(g, df , q)
  structure(g, class = c("fwd", class(g)))
}

new_tree <- function(df) {
  adj    <- make_null_graph(colnames(df))
  g      <- new_gengraph(df, adj)
  g$CG   <- as_adj_lst(adj)
  g$WGT  <- tree_weights(g, df) # Weights to use in kruskal procedure
  structure(g, class = c("tree", class(g)))
}

new_edge <- function(e = character(0), d_qic = numeric(0), idx = integer(0), ins = vector("integer", 2L)) {
  # e     : edge to be deletede or added
  # d_aic : entropy difference in the two competing models
  # idx   : in fwd procedure this is the index in MSI where e lives
  # ins   : in fwd procedure this is the indicies in CG where a new clique must be inserted
  structure(e, d_qic = d_qic, idx = idx, ins = ins)
}

#' Fit a decomposable graphical model
#' @description A generic method for structure learning in decomposable graphical models
#' @param df data.frame
#' @param type character ("fwd", "bwd", "tree")
#' @param adj A userspecified adjacency list
#' @param q Penalty term in the stopping criterion (\code{0} = AIC and \code{1} = BIC)
#' @param ... Not used (for extendibility)
#' @return A \code{gengraph} object
#' @examples
#' d <- tgp_dat[1:100, 5:8]
#' gengraph(d)
#' @seealso \code{\link{adj_lst.gengraph}}, \code{\link{adj_mat.gengraph}}, \code{\link{fit_graph}}, \code{\link{step.fwd}}, \code{\link{step.bwd}}
#' @export
gengraph <- function(df, type = "fwd", adj = NULL, q = 0.5, ...) {
  switch(type,
    "fwd"  = new_fwd(df, adj, q),
    "bwd"  = new_bwd(df, adj, q),
    "tree" = new_tree(df)
  ) 
}
