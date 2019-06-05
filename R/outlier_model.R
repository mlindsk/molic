.split_chars <- function(x) unlist(strsplit(x, ""))

## Shut up CRAN check - foreach must know z!
utils::globalVariables('z') 

#' Simulation of TY
#'
#' This function simulates observations Ty in order to obtain the approximated density of TY
#' 
#' @param A Character Matrix (data)
#' @param C_marginals Clique marginal tables
#' @param S_marginals Separator marginal tables
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @export
sim_TY <- function(A,                      # Character Matrix
                  C_marginals,
                  S_marginals,
                  nsim            = 1000,
                  ncores          = 1) {
  # OUTPUT: Simulated TY values of cells from the database given by A
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
  y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim, .combine = 'c', .inorder = FALSE), {
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
    TY(structure(y_sim_z, names = Delta), C_marginals, S_marginals)    
  })
  doParallel::stopImplicitCluster()
  y
}

#' Simulation of cells in contingency tables
#'
#' This function simulates cells in a contingency table based on a decomposable graphical model
#' 
#' @param A Character Matrix (data)
#' @param C_marginals Clique marginal tables
#' @param S_marginals Separator marginal tables
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @export
sim_Y <- function(A,                      # Character Matrix
                  C_marginals,
                  S_marginals,
                  nsim            = 1000,
                  ncores          = 1) {
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
  y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim, .inorder = FALSE), {
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
    structure(y_sim_z, names = Delta)
  })
  doParallel::stopImplicitCluster()
  y
}


#' Outlier tests in contingency tables using decomposable graphical models
#'
#' Outlier tests in contingency tables using decomposable graphical models
#' 
#' @param df Data frame
#' @param adj Adjacency list
#' @param nsim Number of simulations
#' @param ncores Number of cores to use in parallelization
#' @param meta_name A meta name to keep track of different outlier models
#' @export
outlier_model <- function(df,
                          adj       = NULL,
                          nsim      = 1000,
                          ncores    = 1,
                          meta_name = "") {
  ## Comments:
  ## ---------
  ## It is ASSUMED that all values _for all variables_ in df are represented as a single character
  ## - Maybe we will implement a conversion, e.g. levels(df$x1) <- letters[1:length(levels(df$x1))]
  A     <- as.matrix(df)
  RIP   <- rip(adj)
  C     <- RIP$C
  S     <- RIP$S
  Cms   <- a_marginals(A, C)
  Sms   <- a_marginals(A, S)
  sims  <- sim_TY(A, Cms, Sms, nsim = nsim, ncores = ncores)
  mu    <- NA
  sigma <- NA
  mu_hat    <- mean(sims)
  sigma_hat <- stats::var(sims)
  cdf       <- stats::ecdf(sims)
  out <- structure(class = "outlier_model",
    list(
      df          = df,
      meta_name   = meta_name,
      sims        = sims,
      mu          = mu,
      sigma       = sigma,
      mu_hat      = mu_hat,
      sigma_hat   = sigma_hat,
      cdf         = cdf,
      C_marginals = Cms,
      S_marginals = Sms
    )
  )
  return(out)
}

print.outlier_model <- function(x, ...) {
  cat("\n Meta: ", x$meta_name,
    "\n", paste(rep("-", 16), collapse = ""),
    "",
    paste(rep("-", nchar(x$meta_name)), collapse = ""),
    "\n  Simulations:",         length(x$sims),
    "\n  Variables:",           ncol(x$df),
    "\n  Observations:",        nrow(x$df),
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

pmf.outlier_model <- function(x, ...) {
  graphics::hist(x$sims, breaks = 30, xlab = "T(y)", main = x$meta_name, freq = FALSE)
}

cdf <- function(x, ...) {
  UseMethod("cdf")
}

cdf.outlier_model <- function(x, ...) {
  x$cdf
} 

p_val <- function(x, ty_new, ...) {
  UseMethod("p_value")
}

p_value.outlier_model <- function(x, ty_new, ...) {
  1 - x$cdf( ty_new )
}

## outlier_test <- function(x, z) {
##   UseMethod("outlier_test")
## }

## outlier_test.outlier_model <- function(x, z) {
##   # z : the row in df for the observation to be tested.
##   ty_z <- x$sims[z]
##   pval <- p_value(x, ty_z)
##   # if( verbose )
##   pval
## }

## mean.outlier_model <- function(x, ...) {
##   x$mu
## }

## variance <- function(x) {
##   UseMethod("variance")
## }

## variance.outlier_model <- function(x, ...) {
##   x$sigma
## }
