#' Fit a decomposable graphical model
#' @description A generic method for structure learning in decomposable graphical models
#' @param df data.frame
#' @param type Character ("fwd", "bwd", "tree")
#' @param adj Adjacency list of a decomposable graph
#' @param q Penalty term in the stopping criterion (\code{0} = AIC and \code{1} = BIC)
#' @param trace Logical indicating whether or not to trace the procedure
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy.
#' @param wrap logical specifying if the result of a run with type = "tree" should be converted to a "fwd" object
#' @return A \code{gengraph} object with values:
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' ## All handwritten digits that have true class equal to a "1".
#' d <- digits %>%
#'   filter(class == "1") %>%
#'   select(-class) 
#'
#' g <- fit_graph(d)
#' print(g)
#' plot(g, vertex.size = 1)
#' adj_mat(g)
#' adj_lst(g)
#' }
#' @references \url{https://arxiv.org/abs/1301.2267}, \url{https://doi.org/10.1109/ictai.2004.100} 
#' @seealso \code{\link{adj_lst.gengraph}}, \code{\link{adj_mat.gengraph}}, \code{\link{walk.fwd}}, \code{\link{walk.bwd}}, \code{\link{gengraph}}
#' @export
fit_graph <- function(df,
                      type  = "fwd",
                      adj   = NULL,
                      q     = 0.5,
                      trace = TRUE,
                      thres = 5,
                      wrap  = TRUE)
{
  n <- ncol(df)
  if (!(type %in% .types())) stop(.types_msg())
  if (q < 0 || q > 1) stop("q must be between 0 and 1")
  if (n == 1L) {
    adj <- structure(list(character(0)), names = colnames(df))
    x   <- gengraph(df, type = "gen", adj)
    return(x)
  } 

  x <- gengraph(df, type, adj)
  
  if (inherits(x, "fwd")) {
    if (!neq_empt_chr(as.vector(x$e))) {
      # If no edges are added in fwd_init, x$e = character(0)
      if (trace) msg(k, complete, stop_val, "delta-qic")
      return(x)
    }
  }  
  
  if (inherits(x, "tree")) return(fit_tree(x, df, wrap))

  complete <- n * (n-1L) / 2L
  null     <- 0L
  triv     <- trivial(x, null, complete)
  update_k <- update_iteration(x)
  k        <- sum(x$G_A)/2

  if (k == triv) return(x)
  x <- walk(x = x, df = df, q = q, thres = thres)
  k <- update_k(k)

  if (k == triv) return(x)

  stp      <- stop_condition(x)
  stop_val <- attr(x$e, "d_qic")
  if (stp(stop_val)) return(x)

  while (!stp(stop_val)) {
    if (trace) msg(k, complete, stop_val, "delta-qic")
    x <- walk(x = x, df = df, q = q, thres = thres)
    k  <- update_k(k)
    if (k == triv) {
      if (trace) msg(k, complete, stop_val, "delta-qic")
      return(x)
    }
    stop_val <- attr(x$e, "d_qic")
  }
  if (trace) msg(k, complete, stop_val, "delta-qic")
  return(x)
}

