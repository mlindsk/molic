.split_chars <- function(x) unlist(strsplit(x, ""))

## Shut up CRAN check - foreach and z are good friends!
utils::globalVariables('z') 

.sim_internal <- function(A,
                  C_marginals,
                  S_marginals,
                  nsim         = 1000,
                  type         = "lr",
                  ncores       = 1) {
  y <- replicate(nsim, vector("character", ncol(A)), simplify = FALSE)
  M <- nrow(A)
  Delta   <- colnames(A)
  C1_vars <- attr(C_marginals[[1]], "vars")
  C1_idx  <- match(C1_vars, Delta)                                    ## Make a C++ version?
  p_nC1   <- C_marginals[[1]] / M
  yC1_sim <- sample(names(p_nC1), nsim, replace = TRUE, prob = p_nC1) ## C++ version?
  if(!( length(C_marginals) - 1L)) {
    # The complete graph
    yC1_sim <- lapply(strsplit(yC1_sim, ""), function(z) {names(z) = C1_vars; z})
    return( sapply(yC1_sim, TY, C_marginals, S_marginals) )
  }
  doParallel::registerDoParallel(ncores)
  combine_ <- switch(type, "lr"  = 'c', "raw" = "rbind")
  y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim, .combine = combine_, .inorder = FALSE), {
    ## USE ITERATORS INSTEAD OF "z"
    y_sim_z <- y[[z]]
    y_sim_z[C1_idx] <- .split_chars(yC1_sim[1])
    for( k in 2:length(C_marginals) ) {
      nCk     <- C_marginals[[k]]
      Ck_vars <- attr(nCk, "vars")     # Clique names
      Ck_idx  <- match(Ck_vars, Delta) # Where is Ck in Delta
      nSk     <- S_marginals[[k]]      # For Sk = Ø we have that nSk = M
      Sk_vars <- attr(nSk, "vars")     # Separator names
      if( is.null(Sk_vars) ) {
        # For empty separators
        p_nCk_minus_nSk <- nCk / nSk # nSk = M !
        y_sim_z[Ck_idx] <- .split_chars(sample(names(p_nCk_minus_nSk), 1L, prob = p_nCk_minus_nSk))
      } else {
        Sk_idx              <- match(Sk_vars, Delta)
        Sk_idx_in_Ck        <- match(Sk_vars, Ck_vars)
        Ck_idx_minus_Sk_idx <- Ck_idx[-Sk_idx_in_Ck]
        ySk                 <- y_sim_z[Sk_idx]
        nSk_ySk             <- na_ya(nSk, paste0(ySk, collapse = ""))
        nCk_given_Sk        <- n_b(nCk, structure(Sk_idx_in_Ck, names = ySk) )
        p_nCk_given_Sk_ySk  <- nCk_given_Sk / nSk_ySk # Cant be Inf, since ySk MUST be present since we simulated it
        y_sim_z[Ck_idx_minus_Sk_idx] <- .split_chars(sample(names( p_nCk_given_Sk_ySk), 1L, prob =  p_nCk_given_Sk_ySk))
      }
    }
    out <- structure(y_sim_z, names = Delta)
    if ( type == "lr") {
      out <- TY(out, C_marginals, S_marginals)
    }
    out
  })
  doParallel::stopImplicitCluster()
  y
}

#' Simulate observations from a decomposable graphical model
#'
#' This function simulates observations from a DGM
#' 
#' @param A Character Matrix (data)
#' @param adj Adjacency list of a decomposable graph
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @export
dgm_sim <- function(A, adj, nsim = 1000, ncores = 1) {
  stopifnot( is.matrix(A) )
  RIP   <- rip(adj) # the rip (or actually mcs) will check for decomposability here
  Cms   <- a_marginals(A, RIP$C)
  Sms   <- a_marginals(A, RIP$S)
  .sim_internal(A, Cms, Sms, nsim = nsim, type = "raw", ncores = ncores)
}

#' Outlier model
#'
#' A model based on decomposable graphical models for outlier detection
#'
#' @param A Character Matrix (data)
#' @param adj Adjacency list of a decomposable graph
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param meta_name A meta name to keep track of different outlier models
#' @param validate_A If TRUE, it is checked if all values in A are characters with \code{nchar == 1} (which is required)
#'
#' @details It is assumed that all cell values in \code{A}, for all variables,
#' are represented as a single character. If \code{validate_A} is \code{TRUE} this is checked.
#' If cell values are not single characters, one may exploit \code{letters} and \code{LETTERS} e.g.
#' @export
outlier_model <- function(A,
                          adj,
                          nsim       = 1000,
                          ncores     = 1,
                          meta_name  = "",
                          validate_A = TRUE) {
  stopifnot( is.matrix(A) )
  if ( validate_A ) {
    ## All values _for all variables_ in A must be represented as a single character
    for( i in seq_along(nrow(A)) ) {
      for( j in seq_along(ncol(A)) )
        stopifnot( nchar(A[i,j]) == 1L )
    }
  }
  RIP   <- rip(adj) # the rip (or actually mcs) will check for decomposability here
  Cms   <- a_marginals(A, RIP$C)
  Sms   <- a_marginals(A, RIP$S)
  sims  <- .sim_internal(A, Cms, Sms, nsim = nsim, type = "lr", ncores = ncores)
  mu    <- NA
  sigma <- NA
  mu_hat    <- mean(sims)
  sigma_hat <- stats::var(sims)
  cdf       <- stats::ecdf(sims)
  out <- structure(class = "outlier_model",
    list(
      A           = A,
      meta_name   = meta_name,
      sims        = sims,
      mu          = mu,
      sigma       = sigma,
      mu_hat      = mu_hat,
      sigma_hat   = sigma_hat,
      cdf         = cdf,
      Cms         = Cms,
      Sms         = Sms
    )
  )
  return(out)
}

#' Print outlier model
#'
#' A print method for \code{outlier_model} objects
#'
#' @param x A \code{outlier_model} object
#' @param ... Not used (for S3 compatability)
#' @export
print.outlier_model <- function(x, ...) {
  cat("\n Meta: ", x$meta_name,
    "\n", paste(rep("-", 16), collapse = ""),
    "",
    paste(rep("-", nchar(x$meta_name)), collapse = ""),
    "\n  Simulations:",         length(x$sims),
    "\n  Variables:",           ncol(x$A),
    "\n  Observations:",        nrow(x$A),
    "\n  Theoretical mean:",    round(x$mu, 2),
    "\n  Theoretical variance:",round(x$sigma, 2),
    "\n  Estimated mean:",      round(x$mu_hat, 2),
    "\n  Estimated variance:",  round(x$sigma_hat, 2),
    "\n  <outlier_model>", 
    "\n ---------------",
    "\n\n"
  )
}


pmf <- function(x, ...) {
  UseMethod("pmf")
}

#' pmf
#'
#' A plot method to show the pmf of the approximated pmf of \code{T(Y)}
#'
#' @param x A \code{outlier_model} object
#' @param ... Not used (for S3 compatability)
#' @export
pmf.outlier_model <- function(x, ...) {
  graphics::hist(x$sims, breaks = 30, xlab = "T(y)", main = x$meta_name, freq = FALSE)
}

cdf <- function(x, ...) {
  UseMethod("cdf")
}

#' cdf
#'
#' The empirical cdf function of \code{T(Y)}
#'
#' @param x A \code{outlier_model} object
#' @param ... Not used (for S3 compatability)
#' @export
cdf.outlier_model <- function(x, ...) {
  x$cdf
} 

p_val <- function(x, ty_new, ...) {
  UseMethod("p_val")
}

#' p_val
#'
#' Calculate the p value for obtaining ty_new under \code{H_0}
#'
#' @param x A \code{outlier_model} object
#' @param ty_new The transformed value T(y_new) obtained from function \code{TY}
#' @param ... Not used (for S3 compatability)
#' @export
p_val.outlier_model <- function(x, ty_new, ...) {
  1 - x$cdf( ty_new )
}

#' mean
#'
#' Estimated mean for T(Y)
#'
#' @param x A \code{outlier_model} object
#' @param ... Not used (for S3 compatability)
#' @export
mean.outlier_model <- function(x, ...) {
  x$mu_hat
}

variance <- function(x) {
  UseMethod("variance")
}

#' variance
#'
#' Estimated variance for T(Y)
#'
#' @param x A \code{outlier_model} object
#' @param ... Not used (for S3 compatability)
#' @export
variance.outlier_model <- function(x, ...) {
  x$sigma_hat
}
