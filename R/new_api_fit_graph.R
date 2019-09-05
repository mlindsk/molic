is_trivial <- function(x, k, null, complete) {
  # x: gengraph
  if( inherits(x, "bwd") ) return( k == null) 
  if( inherits(x, "fwd") ) return( k == compelte)
}

fit_graph <- function(df,
                      type  = "bwd", # (fwd, bwd, tree)
                      adj   = NULL,
                      q     = 0.5,
                      trace = TRUE,
                      thres = 5,
                      mem   = new.env(hash = TRUE))
{
  # if ( type == "tree" ) return(fit_tree(df))
  # if ( type == "mix"  ) do something
  x <- gengraph(df, adj, type)
  n <- ncol(df)
  if ( n < 2 ) stop("df must have at least two variables")
  if ( q < 0 || q > 1 ) stop("p must be between 0 and 1")
  complete <- n * (n-1L) / 2L
  null     <- 0L
  k        <- sum(x$G_A)/2
  if ( k == null ) return(x) # use is_trivial
  x  <- step(x = x, df = df, q = q, thres = thres)
  k  <- k - 1L
  if ( k == null ) return(x)
  stop_val  <- attr(x$e, "d_qic")
  if (stop_val >= 0 ) return(x)
  while (stop_val < 0) {
    if (trace) msg(k, complete, stop_val, "delta-qic")
    x <- step(x = x, df = df, q = q, thres = thres)
    k <- k - 1L
    if( k == null ) {
      if (trace) msg(k, complete, stop_val, "delta-qic")
      return(x)
    }
    stop_val <- attr(x$e, "d_qic")
  } 
  return(x)
}

## d <- tgp_dat[1:1000, 5:10]
## g <- fit_graph(d)
## plot(g)
