new_gengraph <- function(df, adj) {
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

new_bwd <- function(df, adj = NULL) {
  if(is.null(adj)) adj <- make_complete_graph(colnames(df))
  g    <- new_gengraph(df, adj)
  g$CG <- rip(adj)$C
  g$e  <- NULL # The newly deleted edge
  structure(g, class = c("bwd", class(g)))
}

new_fwd <- function(df, adj = NULL) {
  if(is.null(adj)) adj <- make_null_graph(colnames(df))
  g      <- new_gengraph(df, adj)
  g$CG_A <- if( is_graph_null(g) ) as_adj_mat(adj) else stop("Not implementet for general graphs yet.") # Fix this to handle the conversion
  g$MSI  <- list(S = NULL, max = list(e = character(0), idx = numeric(0), ins = vector("numeric", 2L))) # EFS structure
  structure(g, class = c("fwd", class(g)))
}

new_tree <- function(df) {
  adj    <- make_null_graph(colnames(df))
  g      <- new_gengraph(df, adj)
  g$CG   <- as_adj_lst(adj)
  g$WGT  <- tree_weights(g, df) # Weights to use in kruskal procedure
  structure(g, class = c("tree", class(g)))
}

# Export this constructor
gengraph <- function(df, type = "fwd", adj = NULL) {
  switch(type,
    "fwd"  = new_fwd(df, adj),
    "bwd"  = new_bwd(df, adj),
    "tree" = new_tree(df)
  ) 
}
