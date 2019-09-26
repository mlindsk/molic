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
#' \dontrun{
#' library(dplyr)
#' 
#' # All handwritten digits that have true class equal to a "1".
#' d <- digits %>% # subset(digits, class == "1")
#'   filter(class == "1") %>%
#'   select(-class)
#'
#' # A handwritten digit with true class equal to "7"
#' z <- digits %>%
#'   filter(class == "7") %>%
#'   select(-class) %>%
#'   slice(1) %>%
#'   unlist()
#'
#' # Fit an interaction graph
#' G <- fit_graph(d, trace = FALSE)
#' plot(G, vertex.size = 1.5)
#' 
#' # Append z to the class og "1" digits
#' dz <- rbind(d, z)
#' 
#' # The outlier model
#' set.seed(7)
#' m <- outlier_model(as.matrix(dz), adj_lst(G))
#' print(m)
#' pmf(m)
#' 
#' # z is declared as an outlier in the "1" class on a 0.05 significance level
#' dvz <- deviance(m, z)
#' pval(m, dvz)
#' }
#' @export
outlier_model <- function(A,
                          adj,
                          nsim       = 5000,
                          ncores     = 1,
                          validate   = TRUE
                          ) {
  stopifnot( is.matrix(A) )
  if (validate ) if( !only_single_chars(A)) stop("All values in A must be represented as a single character. Use to_single_chars(A)")
  RIP   <- rip(adj) # the rip (or actually mcs) will check for decomposability here
  Cms   <- a_marginals(A, RIP$C)
  Sms   <- a_marginals(A, RIP$S)
  sims  <- .sim_internal(A, Cms, Sms, nsim = nsim, type = "deviance", ncores = ncores)
 mu    <- mean(sims)
  sigma <- stats::var(sims)
  cdf   <- stats::ecdf(sims)
  return(new_outlier_model(A, sims, mu, sigma, cdf, Cms, Sms))
}

#' Fit Outlier
#'
#' A convinient wrapper around the \code{outlier_model} for doing outlier test
#'
#' @param A Character Matrix (data)
#' @param z Named vector (same names as \code{colnames(A)})
#' @param adj Adjacency list of a decomposable graph
#' @param alpha The significance level
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. If true, it checks if \code{A} has only single character values and converts it if not.
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' # All handwritten digits that have true class equal to a "1".
#' d <- digits %>%
#'   filter(class == "1") %>%
#'   select(-class)
#'
#' # A handwritten digit with true class equal to "7"
#' z <- digits %>%
#'   filter(class == "7") %>%
#'   select(-class) %>%
#'   slice(1) %>%
#'   unlist()
#'
#' # Fit an interaction graph
#' G <- fit_graph(d, trace = FALSE)
#' plot(G, vertex.size = 1.5)
#' 
#' # Test if z is an outlier
#' m <- fit_outlier(as.matrix(d), z, adj_lst(G))
#' print(m)
#' pmf(m)
#' }
#' @export
fit_outlier <- function(A, z, adj, alpha = 0.05, nsim = 5000, ncores = 1, validate = TRUE) {
  if (all(colnames(A) != names(z))) stop("Variables in A and the names of z is not in agreement!")
  d_z   <- rbind(A, z)
  if (validate) {
    if( !only_single_chars(d_z) ) {
      message("  Note: A has values larger than a single character. to_single_chars() was used to correct this")
      d_z <- to_single_chars(d_z)
      z   <- d_z[nrow(d_z), ]
    }    
  }
  m     <- outlier_model(d_z, adj, nsim = nsim, ncores = ncores, validate = FALSE)
  dev_z <- deviance(m, z)
  m     <- new_outlier(m, dev_z, pval(m, dev_z), critval(m, alpha), alpha)
  return(m)
}

#' Mixed Outlier Test
#'
#' A convinient function for outlier detection with mixed information
#'
#' @param m1 A \code{outlier_model} object
#' @param m2 A \code{outlier_model} object
#' @param z1 Named vector (same names as \code{colnames(A)})
#' @param z2 Named vector (same names as \code{colnames(A)})
#' @param alpha The significance level
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. If true, it checks if \code{A} has only single character values and converts it if not.
#' @examples
#' # See the package vignette "Outlier Detection in Genetic Data"
#' @export
fit_mixed_outlier <- function(m1, m2, z1, z2, alpha = 0.05, nsim = 5000, ncores = 1, validate = TRUE) {
  m     <- convolute(m1, m2)
  dev_z <- deviance(m1, z1) + deviance(m2, z2)
  m     <- new_mixed_outlier(m, dev_z, pval(m, dev_z), critval(m, alpha), alpha)
  return(m)
}
