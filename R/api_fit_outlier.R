#' Outlier model
#'
#' A model based on decomposable graphical models for outlier detection
#'
#' @param A Character Matrix (data)
#' @param adj Adjacency list of a decomposable graph
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. See details.
#'
#' @details It is assumed that all cell values in \code{A}, for all variables,
#' are represented as a single character. If \code{validate} is \code{TRUE} this is checked.
#' If cell values are not single characters, one may exploit the \code{to_single_chars} function
#' @examples
#'
#' # All handwritten digits that have true class equal to a "1".
#' d <- subset(digits, class == "1")
#' d[, "class"] <- NULL
#'
#' # A handwritten digit with true class equal to "7"
#' zd <- subset(digits, class == "7")
#' z  <- unlist(zd[1, 1:(ncol(zd)-1)])
#'
#' # Fit an interaction graph
#' G <- fit_graph(d)
#' plot(G, vertex.size = 1)
#' 
#' # Append z to the class og "1" digits
#' dz <- rbind(d, z)
#' 
#' # The outlier model
#' set.seed(7)
#' M <- outlier_model(as.matrix(dz), adj_lst(G))
#' M
#' pmf(M)
#' 
#' # z is declared as an outlier in the "1" class on a 0.05 significance level
#' dvz <- deviance(M, z)
#' pval(M, dvz)
#' @export
outlier_model <- function(A,
                          adj,
                          nsim       = 5000,
                          ncores     = 1,
                          validate   = TRUE
                          ) {
  stopifnot( is.matrix(A) )
  if (validate ) if( !only_1chars(A)) stop("All values in A must be represented as a single character. Use to_single_chars(A)")
  RIP   <- rip(adj) # the rip (or actually mcs) will check for decomposability here
  Cms   <- a_marginals(A, RIP$C)
  Sms   <- a_marginals(A, RIP$S)
  sims  <- .sim_internal(A, Cms, Sms, nsim = nsim, type = "deviance", ncores = ncores)
  mu    <- mean(sims)
  sigma <- stats::var(sims)
  cdf   <- stats::ecdf(sims)
  return(new_outlier_model(A, sims, mu, sigma, cdf, Cms, Sms))
}


#' Outlier Test
#'
#' A convinient wrapper around the \code{outlier_model} for outlier detection
#'
#' @param A Character Matrix (data)
#' @param z Named vector (same names as \code{colnames(A)})
#' @param adj Adjacency list of a decomposable graph
#' @param type Character ("fwd", "bwd", "tree")
#' @param alpha The significance level
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. If true, it checks if \code{A} has only single character values and converts it if not.
#' @examples
#' # All handwritten digits that have true class equal to a "1".
#' d <- subset(digits, class == "1")
#' d[, "class"] <- NULL
#'
#' # A handwritten digit with true class equal to "7"
#' zd <- subset(digits, class == "7")
#' z  <- unlist(zd[1, 1:(ncol(zd)-1)])
#'
#' # Fit an interaction graph
#' G <- fit_graph(d)
#' plot(G, vertex.size = 1)
#'
#' # Test if z is an outlier
#' M <- fit_outlier(as.matrix(d), z, adj_lst(G))
#' M
#' pmf(M)
#' @export
fit_outlier <- function(A, z, adj, type = "fwd", alpha = 0.05, nsim = 5000, ncores = 1, validate = TRUE) {
  if (all(colnames(A) != names(z))) stop("Variables in A and the names of z is not in agreement!")
  d_z   <- rbind(A, z)
  if (validate) {
    if( !only_1chars(d_z) ) {
      message("  Note: A has values larger than a single character. to_single_chars() was used to correct this")
      d_z <- to_single_chars(d_z)
      z   <- d_z[nrow(d_z), ]
    }    
  }
  m     <- outlier_model(d_z, adj, nsim = nsim, ncores = ncores, validate = FALSE)
  dev_z <- deviance(m, z)
  m     <- new_outlier(m, dev_z, pval(m, dev_z), critval(m), alpha)
  return(m)
}
