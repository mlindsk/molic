#' Fit a decomposable graphical model
#' @description A generic method for structure learning in decomposable graphical models
#' @param df data.frame
#' @param type character ("fwd", "bwd", "tree")
#' @param adj A userspecified adjacency list
#' @param q Penalty term in the stopping criterion (\code{0} = AIC and \code{1} = BIC)
#' @param trace Logical indicating whether or not to trace the procedure
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy.
#' @param wrap logical specifying if the result of a run with type = "tree" should be converted to a "fwd" object
#' @return A \code{gengraph} object with values:
#' @examples
#' d <- tgp_dat[1:100, 5:8]
#' g <- fit_graph(d)
#' # plot(g)
#' # adj_mat(g)
#' # adj_lst(g)
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100} 
#' @seealso \code{\link{adj_lst.gengraph}}, \code{\link{adj_mat.gengraph}}, \code{\link{step.fwd}}, \code{\link{step.bwd}}, \code{\link{gengraph}}
#' @export
fit_graph <- function(df,
                      type  = "fwd", # (fwd, bwd, tree)
                      adj   = NULL,
                      q     = 0.5,
                      trace = TRUE,
                      thres = 5,
                      wrap  = TRUE)
{
  x <- gengraph(df, type, adj)
  if( inherits(x, "tree") ) return(fit_tree(x, df, wrap))
  n <- ncol(df)
  if ( n < 2 ) stop("df must have at least two variables")
  if ( q < 0 || q > 1 ) stop("p must be between 0 and 1")
  complete <- n * (n-1L) / 2L
  triv     <- trivial(x, 0L, complete)
  update_k <- update_iteration(x)
  k        <- sum(x$G_A)/2
  if ( k == triv ) return(x)
  x   <- step(x = x, df = df, q = q, thres = thres)
  k   <- update_k(k)
  stp <- stop_condition(x)
  if ( k == triv ) return(x)
  stop_val  <- attr(x$e, "d_qic")
  if (stp(stop_val) ) return(x)
  while ( !stp(stop_val) ) {
    if (trace) msg(k, complete, stop_val, "delta-qic")
    x <- step(x = x, df = df, q = q, thres = thres)
    k  <- update_k(k)
    if( k == triv ) {
      if (trace) msg(k, complete, stop_val, "delta-qic")
      return(x)
    }
    stop_val <- attr(x$e, "d_qic")
  } 
  return(x)
}

## d <- tgp_dat[1:1000, 5:10]
## g <- fit_graph(d, type = "fwd")
## plot(g)
