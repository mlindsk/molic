outlier_component <- function(meta_pop,
                          df,
                          G, 
                          C_marginals,
                          S_marginals,
                          nsim    = 1000,
                          nc      = 1,
                          verbose = TRUE,
                          moment1 = TRUE,
                          moment2 = FALSE) {
  # INPUT:
  # meta_pop     : A string with the name of the meta population
  # df           : A tibble/data.frame with observed DNA profiles - each being a factor!
  #                in wide format (variables as columns). Only for one chromosome
  #                - factor levels can be used to specify structural zeroes or unseen outcomes
  # G            : igraph object
  # nsim         : number of simulations
  # nc           : number of cores for parallelization

  # If structural zeroes are needed, create the factors before running the function
  # with the extra levels as needed.

  # OUTPUT:
  # An outlier model for the component G

  # For now: Make the conversion
  df[] <- lapply(df, as.factor)

  sims <- sim_TY(df,
    C_marginals,
    S_marginals,
    n.sim   = nsim,
    ncores  = nc,
    verbose = verbose
  )

  mu <- NA
  sigma <- NA
  
  if( moment1 ) mu <- E_TY(df, C_marginals, S_marginals)
  if( moment1 && moment2 ) sigma <- V_TY(df, C_marginals, S_marginals, mu)
  
  mu_hat      <- mean(sims)
  sigma_hat   <- var(sims)
  cdf         <- ecdf(sims)
  
  structure(class = "outlier_component",
    list(
      meta_pop    = meta_pop,
      G           = G,
      sims        = sims,
      mu          = mu,
      sigma       = sigma,
      mu_hat      = mu_hat,
      sigma_hat   = sigma_hat,
      cdf         = cdf,
      C_marginals = C_marginals,
      S_marginals = S_marginals,
      dfs         = df,
      mus         = mu,
      sigmas      = sigma
    )
  )
}


#' Outlier tests in decomposable graphical models
#' 
#' @export
outlier_model <- function(df,
                          component_list,
                          interaction_graph  = efs_graph(df, component_list),
                          meta_pop           = "",
                          nsim               = 1000,
                          nc_component       = 1,
                          nc_simulation      = 1, 
                          verbose_component  = TRUE,
                          verbose_simulation = FALSE,
                          moment1            = TRUE,
                          moment2            = FALSE) {
  ## INPUT:
  ## -------
  ## component_list    : A NAMED list of components with each element being a character of
  ##                     - of component nodes
  ## interaction_graph : A named list of igraph objects corresponding to the components
  ##                    - Build in types are "mst" and the saturated model "sat"
  ##                    - In the future chau liu is implemented
  ## meta_pop          : Just a name for keeping track of the meta population under H0.
  ## nc_component      : number of cores to split between model_components
  ## nc_simulation     : number of cores to split on each simulation in model_components
  ## moments           : should the theoretical mean(1) and variance(2) be calculated

  ## OUTPUT:
  ## -------
  ## A superposition (+) of outerlier_components

  ## NOTE:
  ## -----
  ## The user should tjeck if the components of interaction_graph are decomposable
  ## - use the auxillary function is_decomposable
  ## - it will NOT be checked during the procedure
  ## Note that all interaction graphs need to be connected!
  ## - All interaction graphs can be seen as components of the "meta graph"

  ## Tracing
  if( !verbose_component ) pbapply::pboptions(type = "none")
    on.exit(pbapply::pboptions(type = "timer"))
  ## Exception Handling
  ## igraph::is.igraph(g)), igraph::is.connected(g)), igraph::is_weighted(g))

  variables_in_df <- colnames(df)
  component_models <- pbapply::pblapply(X = 1:length(component_list),
    cl = nc_component,
    FUN = function(k) {
      x    <- component_list[[k]]
      if( !all(x %in% variables_in_df) ) stop("component_list and columns in data do not agree")
      df_x <- df[, x]
      G    <- interaction_graph[[k]]
      H    <- make_vgraph(G) # Maybe the default graphs should just produce these in advance?
      # A vanilla representation of edges and vertices in G (istead of igraph)
      # - Easier to port to C++ in the future and more lightweight
      rip  <- rip(H)
      C    <- rip$C
      S    <- rip$S
      Cm   <- a_marginals(df_x, C) # Slow when the number of different alleles increases
      Sm   <- a_marginals(df_x, S) # Implement na2 : the equvivalent to entropy2 in efs_utils.R
      outlier_component(meta_pop = meta_pop,
        df            = df_x,
        G             = G,
        C_marginals   = Cm,
        S_marginals   = Sm,
        nsim          = nsim,
        nc            = nc_simulation,
        verbose       = verbose_simulation,
        moment1       = moment1,
        moment2       = moment2
      )
    }
  )
  out <- structure(Reduce(`+`, component_models), class = "outlier_model")
  if( length(component_list) == 1 ) {
    out$dfs         <- list(out$dfs)
    out$C_marginals <- list(out$C_marginals)
    out$S_marginals <- list(out$S_marginals)
  } 
  out$df            <- df
  out$component_list<- component_list
  return(out)
}

print.outlier_model <- function(x, ...) {
  cat("\n Meta population: ", x$meta_pop,
    "\n", paste(rep("-", 16), collapse = ""),
    "",
    paste(rep("-", nchar(x$meta_pop)), collapse = ""),
    "\n  Simulations:",         length(x$sims),
    "\n  Components:",          length(x$component_list),
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

plot.outlier_model <- function(x,
                               y,
                               form,
                               weights = FALSE,
                               lay = "nice",
                               ...) {
  # y: vector with component indicies or component name
  # form: c(a,b) for #a rows and #b columns
  lay_ <- switch(lay,
    "nice"   = igraph::layout_nicely,
    "tree"   = igraph::layout_as_tree,
    "circle" = igraph::layout_in_circle
  )

  comps <- igraph::components(x$G)
  memb  <- comps$membership
  comps <- split(memb, memb)
  names(comps) <- names(x$component_list)
  vsub  <- unname(unlist(lapply(comps[y], names)))
  Gsub  <- igraph::induced_subgraph(x$G, vsub)
  Gsub_decomposition <- igraph::decompose.graph(Gsub)

  par(mfrow = form,
      oma = c(5,4,0.2,0.2) + 0.1,
      mar = c(1,1,1,1) + 0.3)

  el <- NULL
  # None of the default graphs are weighted, so redundant
  if( weights ) el <- round(igraph::E(Gsub)$weight, 3)
  for( g in seq_along(Gsub_decomposition) ) {
    Gg <- Gsub_decomposition[[g]]
    igraph::plot.igraph(Gg,
                        layout      = lay_,
                        main        = y[g],
                        vertex.size = 0,
                        label.cex   = .1,
                        edge.width  = 2,
                        edge.label  = el
    )
  }
}

pmf <- function(x, ...) {
  UseMethod("pmf")
}

pmf.outlier_model <- function(x, ...) {
  hist(x$sims, breaks = 30, xlab = "T(y)", main = x$meta_pop, freq = FALSE)
}

cdf <- function(x, ...) {
  UseMethod("cdf")
}

cdf.outlier_model <- function(x, ...) {
  x$cdf
} 

## Not neccesarily an .outlier_model class! Fix!
Ty <- function(x, y_new, ...) {
  UseMethod("Ty")
}

Ty.outlier_model <- function(x, y_new, ...) {
  if( anyNA(y_new) ) stop( "y_new has NAs" )
  # Give a warning if y_new has NAs
  # But then, evaluate T(y_new) for all non-missing components?

  ## TYs <- vapply(X =  1:length(x$dfs), # OLD VERSION
  TYs <- vapply(X =  1:length(x$component_list), # Corrected to handle single component models
    FUN.VALUE = 1,
    FUN =  function(j) {
      # TO DO: Tjeck conformality
      ia_y_new <- match(colnames(x$dfs[[j]]), names(y_new))
      ia_y_new <- ia_y_new[!is.na(ia_y_new)]
      T_Y(x$dfs[[j]], y_new[ia_y_new], x$C_marginals[[j]], x$S_marginals[[j]])
    }
  )
  sum(TYs)
}

p_value <- function(x, ty_new, ...) {
  UseMethod("p_value")
}

p_value.outlier_model <- function(x, ty_new, ...) {
  1 - x$cdf( ty_new )
}

mean.outlier_model <- function(x, ...) {
  x$mu
}

variance <- function(x) {
  UseMethod("variance")
}

variance.outlier_model <- function(x, ...) {
  x$sigma
}

joint_component_prob <- function(x, y, k) {
 UseMethod("joint_component_prob")
}

## Not neccesarily an .outlier_model class! - Fix?
## We can use this to investigate local information!
joint_component_prob.outlier_model <- function(x, y, k = seq_along(x$C_marginals)) {
  ## For futher marginal probabilities we need to propagate
  ## k: integer or character specifying which components to obtain probabilities from
  ## y: the profile
  if( anyNA(y) ) stop( "y has NAs" )
  df <- x$df
  ps <- sapply(k, function(j) {
    p_y(df, y, x$C_marginals[[j]], x$S_marginals[[j]] )
  })
  prod(ps)
}


`+.outlier_component` = function(e1, e2) {
  # Adding components
  meta_pop    <- e1$meta_pop
  G           <- igraph::union(e1$G, e2$G)
  sims        <- e1$sims + e2$sims
  mu          <- e1$mu + e2$mu
  sigma       <- e1$sigma + e2$sigma
  mu_hat      <- e1$mu_hat + e2$mu_hat
  sigma_hat   <- e1$sigma_hat + e2$sigma_hat
  cdf         <- ecdf(sims)
  Cm_e1       <- e1$C_marginals
  Cm_e2       <- e2$C_marginals
  Sm_e1       <- e1$S_marginals
  Sm_e2       <- e2$S_marginals
  # Addition of marginals is therefore NOT commutative! Cm_e2[[1]] cannot be a list
  # Change in a future version (Its ok; we Reduce() from "left").
  .listUs <- function(e1_, e2_) {
    if( !is.list(e1_[[1]]) && !is.list(e2_[[1]]) ) {
      e1_ <- list(e1_, e2_)
    } else {
      e1_[[length(e1_) + 1]] <- e2_
    }
    e1_
  }
  ## Saving all component info
  Cm     <- .listUs(Cm_e1, Cm_e2)
  Sm     <- .listUs(Sm_e1, Sm_e2)
  dfs    <- .listUs(e1$df, e2$df)
  mus    <- c(e1$mus, e2$mus)
  sigmas <- c(e1$sigmas, e2$sigmas)
  structure(class = "outlier_component",
    list(
      meta_pop    = meta_pop,
      G           = G,
      sims        = sims,
      mu          = mu,
      sigma       = sigma,
      mu_hat      = mu_hat,
      sigma_hat   = sigma_hat,
      cdf         = cdf,
      C_marginals = Cm,
      S_marginals = Sm,
      dfs         = dfs,
      mus         = mus,
      sigmas      = sigmas
    )
  )
}

`+.outlier_model` = function(e1, e2) {
  # Convolution of two homogeneus outlier_models (same graphs, marginals etc.)
  meta_pop       <- e1$meta_pop
  G              <- e1$G
  sims           <- e1$sims + e2$sims
  mu             <- e1$mu + e2$mu
  sigma          <- e1$sigma + e2$sigma
  mu_hat         <- e1$mu_hat + e2$mu_hat
  sigma_hat      <- e1$sigma_hat + e2$sigma_hat
  cdf            <- ecdf(sims)
  Cm             <- e1$C_marginals
  Sm             <- e1$S_marginals
  dfs            <- e1$dfs
  df             <- e1$df
  component_list <- e1$component_list
  structure(class = "outlier_model",
    list(
      meta_pop       = meta_pop,
      G              = G,
      sims           = sims,
      mu             = mu,
      sigma          = sigma,
      mu_hat         = mu_hat,
      sigma_hat      = sigma_hat,
      cdf            = cdf,
      C_marginals    = Cm,
      S_marginals    = Sm,
      dfs            = dfs,
      df             = df,
      component_list = component_list
    )
  )
}
