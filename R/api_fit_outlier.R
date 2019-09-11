#' Outlier model
#'
#' A model based on decomposable graphical models for outlier detection
#'
#' @param A Character Matrix (data)
#' @param adj Adjacency list of a decomposable graph
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validateA Logical. See details.
#'
#' @details It is assumed that all cell values in \code{A}, for all variables,
#' are represented as a single character. If \code{validate_A} is \code{TRUE} this is checked.
#' If cell values are not single characters, one may exploit \code{letters} and \code{LETTERS} e.g.
#' @examples
#'
#'
#' # DNA data for Europeans
#' df <- subset(tgp_dat[1:1006, c(2, 5:10)], pop_meta == "EUR")                
#'
#' # DNA profile from Africa
#' z <- unlist(subset(tgp_dat[, c(2, 5:10)], pop_meta == "AFR")[1, -1])
#'
#' # Fit an interactiongraph
#' G <- adj_lst(fit_graph(df[, -1]))
#'
#' # Append z to df
#' df_z <- rbind(df[, -1], z)
#'
#' # The outlier model
#' set.seed(7)
#' M <- outlier_model(as.matrix(df_z), G)
#'
#' # z is declared as an outlier in the Europe data on a 0.05 significance level 
#' pval(M, deviance(M, z))
#'
#' # Plot the distribution of the deviance
#' # pmf(M)
#' 
#' @export
outlier_model <- function(A,
                          adj,
                          nsim       = 1000,
                          ncores     = 1,
                          validateA  = TRUE
                          ) {

  stopifnot( is.matrix(A) )
  if( validateA ) {
    ## All values _for all variables_ in A must be represented as a single character
    for( i in seq_along(nrow(A)) ) {
      for( j in seq_along(ncol(A)) )
        stopifnot( nchar(A[i,j]) == 1L )
    }    
  }

  # the rip (or actually mcs) will check for decomposability here
  RIP   <- rip(adj) 
  Cms   <- a_marginals(A, RIP$C)
  Sms   <- a_marginals(A, RIP$S)
  sims  <- .sim_internal(A, Cms, Sms, nsim = nsim, type = "deviance", ncores = ncores)
  mu    <- mean(sims)
  sigma <- stats::var(sims)
  cdf   <- stats::ecdf(sims)
  return(new_outlier_model(A, sims, mu, sigma, cdf, Cms, Sms))
}

fit_outlier <- function(df, z, adj = NULL, nsim = 10000, ncores = 1) {
  if( all(colnames(df) != names(z)) ) stop("Variables in df and the names of z is not in agreement!")
  if( !is.null(adj) ) stopifnot(is_decomposable(adj))
  g <- gengraph(df, "fwd", adj)
  if( is.null(adj) ) g <- fit_graph(df, trace = FALSE)
  d_z   <- rbind(df, z)
  m     <- outlier_model(as.matrix(d_z), adj_lst(g), nsim = nsim, ncores = ncores)
  dev_z <- deviance(m, z)
  m     <- new_outlier(m, dev_z, pval(m, dev_z), g)
  return(m)
}


## df   <- subset(tgp_dat[1:1006, c(2, 5:50)], pop_meta == "EUR")
## df  <- df[, -1]
## z    <- unlist(subset(tgp_dat[, c(2, 5:50)], pop_meta == "AFR")[1, -1])
## fit_graph(df)
## o    <- fit_outlier(df, z, nsim = 100, ncores = 2)
## o$pval
## deviance(o)


## df   <- subset(tgp_dat[1:1006, c(2, 5:10)], pop_meta == "EUR")                
## z    <- unlist(subset(tgp_dat[, c(2, 5:10)], pop_meta == "AFR")[1, -1])
## G    <- adj_lst(fit_graph(df[, -1]))
## df_z <- rbind(df[, -1], z)
## M    <- outlier_model(as.matrix(df_z), G)
