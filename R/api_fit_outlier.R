#' Outlier model
#'
#' A model based on decomposable graphical models for outlier detection
#'
#' @param A Character Matrix (data) with the new observation of interest appended
#' @param adj Adjacency list of a decomposable graph without the observation of interest
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. See details.
#' @details It is assumed that all cell values in \code{A}, for all variables,
#' are represented as a single character. If \code{validate} is \code{TRUE} this is checked.
#' If cell values are not single characters, one may exploit the \code{to_single_chars} function
#' @seealso \code{\link{fit_outlier}}, \code{\link{fit_graph}}
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
#' G <- fit_graph(dz, trace = FALSE)
#' plot(G, vertex.size = 1.5)
#'
#' # Append z to the class of "1" digits
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
#' A convenient wrapper for doing outlier test
#'
#' @param A Character Matrix (data) - without the new observation \code{z} appended
#' @param z Named vector (same names as \code{colnames(A)})
#' @param alpha The significance level
#' @param type Character ("fwd", "bwd", "tree" or "tfwd") - see \code{fit_graph}
#' @param q Penalty term in the stopping criterion (\code{0} = AIC and \code{1} = BIC) - see \code{fit_graph}
#' @param comp A list with character vectors. Each elementer in the list is a component in the graph (using expert knowledge)
#' @param adj Adjacency list of a decomposable graph without the observation of interest
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param trace Logical indicating whether or not to trace the procedure
#' @param validate Logical. If true, it checks if \code{A} has only single character values and converts it if not.
#' @seealso \code{\link{outlier_model}}, \code{\link{fit_graph}}
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
#' # Test if z is an outlier in class "1"
#' m <- fit_outlier(as.matrix(d), z)
#' print(m)
#' pmf(m)
#' }
#' @export
fit_outlier <- function(A,
                        z,
                        alpha    = 0.05,
                        type     = "fwd",
                        q        = 0.5,
                        comp     = NULL,
                        nsim     = 10000,
                        ncores   = 1,
                        trace    = FALSE,
                        validate = TRUE) {

  if (all(colnames(A) != names(z))) stop("Variables in A and the names of z is not in agreement!")

  Az   <- rbind(A, z)

  if (validate) {
    if( !only_single_chars(Az) ) {
      message("  Note: A has values larger than a single character. to_single_chars() was used to correct this")
      Az <- to_single_chars(Az)
      z   <- Az[nrow(Az), ]
    }    
  }

  if (trace) cat(" Fitting the graph ... \n  ---------------------------------------- \n")
  Adf <- as.data.frame(A)
  if (!is.null(comp)) {
    adj <- unlist(
      lapply(unname(comp), function(x) {
        adj_lst(fit_graph(Adf[, x, drop = FALSE], type = type,  q = q, trace = trace))
      }),
      recursive = FALSE
    )
  } else {
    adj  <- adj_lst(fit_graph(Adf, type = type,  q = q, trace = trace))  
  }
  
  if (trace) cat(" ---------------------------------------- \n Outlier model in progress ... \n")
  m     <- outlier_model(Az, adj, nsim = nsim, ncores = ncores, validate = FALSE)
  dev_z <- deviance(m, z)
  m     <- new_outlier(m, dev_z, pval(m, dev_z), critval(m, alpha), alpha)
  return(m)
}


#' Mixed Outlier Test
#'
#' A convenient function for outlier detection with mixed information
#'
#' @param m1 An object returned from \code{fit_outlier}
#' @param m2 An object returned from \code{fit_outlier}
#' @examples
#' \dontrun{
#'
#' # See the package vignette "Outlier Detection in Genetic Data"
#' for an introduction to the genetic data used here
#' 
#' library(dplyr)
#'
#' # The components - here microhaplotypes
#' haps <- tgp_haps
#' 
#' # All the Europeans
#' eur  <- tgp_dat %>%
#'   as_tibble() %>%
#'   filter(pop_meta == "EUR")
#' 
#' # Extracting the two databases for each copy of the chromosomes
#' eur_a <- eur %>%
#'   filter(grepl("a$", sample_name))
#' 
#' eur_b <- eur %>%
#'   filter(grepl("b$", sample_name))
#'
#' # Testing if an American is an outlier in Europe
#' 
#' amr <- tgp_dat %>%
#'   as_tibble() %>%
#'   filter(pop_meta == "AMR")
#'
#' 
#' z1  <- amr %>%
#'   filter(grepl("a$", sample_name)) %>% 
#'   select(unname(unlist(haps))) %>%
#'   slice(1) %>%
#'   unlist()
#' 
#' z2  <- amr %>%
#'   filter(grepl("b$", sample_name)) %>% 
#'   select(unname(unlist(haps))) %>%
#'   slice(1) %>%
#'   unlist()
#' 
#' mat_eur_a <- eur_a %>% select(-c(1:2)) %>% as.matrix()
#' mat_eur_b <- eur_b %>% select(-c(1:2)) %>% as.matrix()
#' 
#' m1 <- fit_outlier(mat_eur_a, z1, comp = haps) # consider using more cores (ncores argument)
#' m2 <- fit_outlier(mat_eur_a, z2, comp = haps) # consider using more cores (ncores argument)
#' fit_mixed_outlier(m1,m2)
#' }
#' @export
fit_mixed_outlier <- function(m1, m2) {
  if (m1$alpha != m2$alpha) warning("Significance levels are different for model m1 and m2!")
  m     <- convolute(m1, m2)
  dev   <- m1$dev + m2$dev
  m     <- new_mixed_outlier(m, dev, pval(m, dev), critval(m, m1$alpha), m1$alpha)
  return(m)
}

#' Fit Multiple Models
#'
#' A convenient wrapper function to conduct multiple tests for a single observation
#'
#' @param A A data frame (data) without the new observation \code{z} appended
#' @param z Named vector (same names as \code{colnames(A)} but without the class variable)
#' @param response A character with the name of the class variable of interest
#' @param alpha The significance level
#' @param type Character ("fwd", "bwd", "tree" or "tfwd") - the type of interaction graph to be used
#' @param q Penalty term in the stopping criterion when fitting the interaction graph (\code{0} = AIC and \code{1} = BIC)
#' @param adj Adjacency list of a decomposable graph without the observation of interest with length equal to the number of unique levels of the response
#' @param comp A list with character vectors. Each elementer in the list is a component in the graph (using expert knowledge)
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param trace Logical indicating whether or not to trace the procedure
#' @param validate Logical. If true, it checks if \code{A} has only single character values and converts it if not.
#' @examples
#' # TBA
#' 1L
#' @export
fit_multiple_models <- function(A,
                                z,
                                response,
                                alpha      = 0.05,
                                type       = "fwd",
                                q          = 0.5,
                                comp       = NULL,
                                nsim       = 10000,
                                ncores     = 1,
                                trace      = TRUE,
                                validate   = TRUE) {
  res_vec  <- A[, response, drop = TRUE]
  res_lvls <- unique(res_vec)
  if (!is.null(adj) && length(adj) != length(res_lvls)) stop("Number of graphs in adj is not equal to the number of levels in the response variable!")
  models <- lapply(seq_along(res_lvls), function(i) {
    if (trace) cat(i, "/", length(res_lvls), " ... \n")
    Ai <- A[res_vec == res_lvls[i], -which(colnames(A) == response)]
    fit_outlier(A = as.matrix(Ai),
      z           = z,
      alpha       = alpha,
      type        = type,
      q           = q,
      comp        = comp,
      nsim        = nsim,
      ncores      = ncores,
      validate    = validate)
  })
  names(models) <- res_lvls
  structure(models, class = c("multiple_models", class(models)))
}
