outlier_model <- function(A,
                          adj,
                          nsim       = 10000,
                          ncores     = 1,
                          validate   = TRUE
                          ) {
  stopifnot(is.matrix(A))
  if (inherits(adj, "gengraph")) adj <- ess::adj_lst(adj)
  if (validate ) {
    if (any(is.na(A))) message("  Note: A has NA values. These have been treated as ordinay values.")
    if( !only_single_chars(A)) stop("All values in A must be represented as a single character. Use to_single_chars(A)")
  }
  RIP   <- ess::rip(adj) # the rip (or actually mcs) will check for decomposability here
  cms   <- a_marginals(A, RIP$C)
  sms   <- a_marginals(A, RIP$S)
  sims  <- .sim_internal(A, cms, sms, nsim = nsim, type = "deviance", ncores = ncores)
  mu    <- mean(sims)
  sigma <- stats::var(sims)
  cdf   <- stats::ecdf(sims)
  return(new_outlier_model(A, sims, mu, sigma, cdf, cms, sms))
}

#' Outlier detection
#'
#' Detecting outliers within a dataset or test if a new (novel) observation is an outlier.
#'
#' @param A Character matrix or data.frame. All values must be limited to a single character.
#' @param adj Adjacency list or \code{gengraph} object of a decomposable graph. See package \code{ess} for \code{gengraph} objects.
#' @param z Named vector (same names as \code{colnames(A)}) or \code{NULL}. See details. Values must be limited to a single character.
#' @param alpha Significance level
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param validate Logical. If true, it checks if \code{A} only has single character values and converts it if not.
#' @details If the goal is to detect outliers within \code{A} set \code{z} to \code{NULL}; this procedure is most
#' often just referred to as outlier detection. Once \code{fit_outlier} has been called in this situation, one can
#' exploit the \code{outliers} function to get the indicies for which observations in \code{A} that are outliers.
#' See the examples.
#'
#' On the other hand, if the goal is test if the new unseen observation \code{z} is an outlier in\code{A},
#' then supply a named vector to \code{z}.
#'
#' All values must be limited to a single character representation; if not, the function will internally convert to one such
#' representation. The reason for this, is a speedup in runtime performance. One can also use the exported
#' function \code{to_single_chars} on \code{A} in advance and set \code{validate} to \code{FALSE}. 
#'
#' The \code{adj} object is most typically found using \code{fit_graph} from the ess package. But the user can supply
#' an adjacency list, just a named \code{list}, of their own choice if needed.
#' @seealso \code{\link{fit_mixed_outlier}}, \code{\link{fit_multiple_models}}, \code{\link{outliers}}
#' @examples
#' \dontrun{
#'
#' library(dplyr)
#' library(ess) # For the fit_graph function
#' 
#' # Psoriasis patients
#' d <- derma %>%
#'   filter(ES == "psoriasis") %>%
#'   select(-ES) %>%
#'   as_tibble()
#'
#' # Fitting the interaction graph
#' g <- fit_graph(d, trace = FALSE) # see package ess for details
#'
#' # -----------------------------------------------------------
#' #                        EXAMPLE 1
#' #    Testing which observations within d are outliers
#' # -----------------------------------------------------------
#'
#' m1 <- fit_outlier(d, g)
#' print(m1)
#' outs  <- outliers(m1, d)
#' douts <- d[which(outs), ]
#' douts
#'
#' # Notice that m1 is of class 'outlier'. This means, that the procedure has tested which
#' # observations _within_ the data are outliers. This method is most often just referred to
#' # as outlier detection. The following plot is the distribution of the test statistic. Think
#' # of a simple t-test, where the distribution of the test statistic is a t-distribution.
#' # In order to conclude on the hypothesis, one finds the critical value and verify if the
#' # test statistic is greater or less than this.
#'
#' # Retrieving the test statistic for the individual observations
#' x1 <- douts[1, ] %>% unlist()
#' x2 <- d[1, ] %>% unlist()
#' dev1 <- deviance(m1, x1) # falls within the critical region in the plot (the red area)
#' dev2 <- deviance(m1, x2) # falls within the acceptable region in the plot
#'
#' dev1
#' dev2
#'
#' 
#' # Retrieving the pvalues
#' pval(m1, dev1)
#' pval(m1, dev2)
#' 
#' # -----------------------------------------------------------
#' #                        EXAMPLE 2
#' #         Testing if a new observation is an outlier
#' # -----------------------------------------------------------
#' 
#' # An observation from class "chronic dermatitis"
#' z <- derma %>%
#'   filter(ES == "chronic dermatitis") %>%
#'   select(-ES) %>%
#'   slice(1) %>%
#'   unlist()
#' 
#' # Test if z is an outlier in class "psoriasis"
#' m2 <- fit_outlier(d, g, z)
#' print(m2)
#' plot(m2) # The vertical dotted line indicates the (deviance) test statistic of z
#'
#' # Notice that m2 is of class 'novelty'. The term novelty detection
#' # is sometimes used in the litterature when the goal is to verify
#' # if a new unseen observation is an outlier in a homogen dataset.
#'
#' # Retrieving the test statistic and pvalue for z
#' dz <- deviance(m2, z)
#' pval(m2, dz)
#' 
#' }
#' @export
fit_outlier <- function(A,
                        adj,
                        z        = NULL,
                        alpha    = 0.05,
                        nsim     = 10000,
                        ncores   = 1,
                        validate = TRUE) {

  # TODO: z may now contain NAs since we are armed with the junction max-flow alg.
  # - in this case, we impute the NAs given the observed (evidence)

  if (!(is.data.frame(A) || is.matrix(A))) stop("A must be either a matrix or a data.frame", call. = FALSE)

  if (any(is.na(A))) stop("A has NA values.")
  
  novelty_detection <- !is.null(z)
  
  if (novelty_detection) {
    if (all(colnames(A) != names(z))) {
      stop("Variables in A and the names of z is not in agreement!")
    }
  }

  if (inherits(adj, "gengraph")) adj <- ess::adj_lst(adj)
  
  if (is.data.frame(A)) A <- as.matrix(A)

  Az <- if (novelty_detection) rbind(A, z) else A

  if (validate) {
    if( !only_single_chars(Az) ) {
      message("A has values longer than a single character. to_single_chars() was used to correct this.")
      Az <- to_single_chars(Az)
      if (novelty_detection) z <- Az[nrow(Az), ]
    }    
  }
  
  m <- outlier_model(Az, adj, nsim = nsim, ncores = ncores, validate = FALSE)

  m <- if (novelty_detection) {
    dev_z <- deviance(m, z)
    new_novelty(m, dev_z, pval(m, dev_z), critval(m, alpha), alpha)
  } else {
    new_outlier(m, critval(m, alpha), alpha)
  }
  
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
#' library(ess)  # for fit_components
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
#'   filter(grepl("a$", sample_name)) %>%
#'   select(-c(1:2))
#' 
#' eur_b <- eur %>%
#'   filter(grepl("b$", sample_name)) %>%
#'   select(-c(1:2))
#'
#' 
#' # Fitting the interaction graphs on the EUR data 
#' ga <- fit_components(eur_a, comp = haps, trace = FALSE, as_gen = TRUE)
#' gb <- fit_components(eur_b, comp = haps, trace = FALSE, as_gen = TRUE)
#'
#' # as_gen = TRUE converts the graph to a gengraph,
#' # the main object for graphs in the ess package.
#' # This enables plotting and print info of the graph:
#' print(ga)
#' plot(ga, vertex.size = 1)
#'
#' # Testing if an American is an outlier in Europe
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
#' m1 <- fit_outlier(eur_a, ga, z1) # consider using more cores (ncores argument)
#' m2 <- fit_outlier(eur_b, gb, z2) # consider using more cores (ncores argument)
#' m  <- fit_mixed_outlier(m1,m2)
#' plot(m)
#' 
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
#' A convenient wrapper function to conduct multiple novelty tests for a new observation
#'
#' @param A A character matrix or data.frame
#' @param z Named vector (same names as \code{colnames(A)} but without the class variable)
#' @param response A character with the name of the class variable of interest
#' @param alpha The significance level
#' @param type Character ("fwd", "bwd", "tree" or "tfwd") - the type of interaction graph to be used
#' @param q Penalty term in the stopping criterion when fitting the interaction graph (\code{0} = AIC and \code{1} = BIC)
#' @param comp A list with character vectors. Each element in the list is a component in the graph (using expert knowledge)
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


  if (!(is.data.frame(A) || is.matrix(A))) stop("A must be either a matrix or a data.frame", call. = FALSE)
  if (is.matrix(A)) A <- as.data.frame(A)
  
  res_vec  <- A[, response, drop = TRUE]
  res_lvls <- unique(res_vec)
  
  models <- lapply(seq_along(res_lvls), function(i) {

    if (trace) cat(i, "/", length(res_lvls), " ... \n")

    Ai <- A[res_vec == res_lvls[i], -which(colnames(A) == response)]

    if (!is.null(comp)) {
      gi <- ess::fit_components(Ai, comp = comp, type = type, q = q, trace = FALSE)
    } else {
      gi <- ess::fit_graph(Ai, type = type, q = q, trace = FALSE)
    }

    fit_outlier(A = Ai,
      adj         = gi,
      z           = z,
      alpha       = alpha,
      nsim        = nsim,
      ncores      = ncores,
      validate    = validate)
    
  })
  
  names(models) <- res_lvls
  structure(models, class = c("multiple_models", class(models)))
}
