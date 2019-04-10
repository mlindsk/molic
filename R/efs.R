efs <- function(df, x = efs_init(df), d = 3, thres = 3, trace = TRUE, log_mdl = TRUE, method = "forward") {
  # INPUT:
  # df: dataframe
  # x: efs object. Default is the null-graph
  # d: number needed to encode a single paramter (see Altmueller)
  # thres: When the size of a set is larger than thres, entropy2 is used for speed
  # - For binary data the "optimal" value of thres is closer to 7
  n        <- ncol(df)
  complete <- n * (n-1L) / 2L
  k        <- length(igraph::E(x$G))
  if( k == complete ) stop("The graph is already complete!")
  prev_mdl <- mdl(x$G_A, df, d, thres)
  x        <- efs_step(df, x, thres)
  curr_mdl <- mdl(x$G_A, df, d, thres)
  k <- k + 1L
  if( curr_mdl > prev_mdl || k == complete) return(x)  
  while( curr_mdl <= prev_mdl ) {
    if( k == complete ) {
      if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(curr_mdl, 6L)), "\n")
      return(x)
    } 
    if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(curr_mdl, 6L)), "\n")
    x    <- efs_step(df, x, thres)
    k    <- k + 1L
    prev_mdl <- curr_mdl
    curr_mdl <- mdl(x$G_A, df, d, thres)
  }
  if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(prev_mdl, 6L)), "\n")
  return(x)
}

print.efs <- function(x, ...) {
  nv <- ncol(x$G_A)
  ne <- length(igraph::E(x$G))
  print(ne)
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$CG),
    "\n  <efs>",
    "\n -------------------------\n"
  )
}
