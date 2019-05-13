## LET df BE A MATRIX WITH DIMNAMES = list(NULL, c(colnames))
## simulate_TY <- function(df,
##                   C_marginals,
##                   S_marginals,
##                   nsim            = 1000,
##                   ncores          = 1,
##                   verbose         = TRUE) {
##   # OUTPUT: Simulated TY values of cells from the database df
##   y <- replicate(nsim, vector("character", ncol(df)), simplify = FALSE)
##   M <- nrow(df)
##   Delta   <- colnames(df)
##   C1_vars <- attr(C_marginals[[1]], "vars")
##   C1_idx  <- match(C1_vars, Delta)
##   p_nC1   <- C_marginals[[1]] / M
##   yC1_sim <- sample(names(p_nC1), nsim, replace = TRUE, prob = p_nC1)
##   if(!( length(C_marginals) - 1L)) {
##     # The complete graph
##     yC1_sim <- lapply(strsplit(yC1_sim, ""), function(z) {names(z) = C1_vars; z})
##     return( sapply(yC1_sim, TY, C_marginals, S_marginals) )
##   } 
##   doParallel::registerDoParallel(ncores)
##   y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim, .combine = 'c'), {
##     y_sim_z <- y[[z]]
##     y_sim_z[C1_idx] <- .split_chars(yC1_sim[1])
##     for( k in 2:length(C_marginals) ) {
##       nCk     <- C_marginals[[k]]
##       Ck_vars <- attr(nCk, "vars")     # Clique names
##       Ck_idx  <- match(Ck_vars, Delta) # Where is Ck in Delta
##       nSk     <- S_marginals[[k]]      # For Sk = Ø we have that nSk = M
##       Sk_vars <- attr(nSk, "vars")     # Separator names
##       if( is.null(Sk_vars) ) {
##         # For empty separators
##         p_nCk_minus_nSk <- nCk / nSk # nSk = M !
##         y_sim_z[Ck_idx] <- .split_chars(sample(names(p_nCk_minus_nSk), 1L, prob = p_nCk_minus_nSk))
##       } else {
##         Sk_idx              <- match(Sk_vars, Delta)
##         Sk_idx_in_Ck        <- match(Sk_vars, Ck_vars)
##         Ck_idx_minus_Sk_idx <- Ck_idx[-Sk_idx_in_Ck]
##         ySk                 <- y_sim_z[Sk_idx]
##         nSk_ySk             <- na_ya(nSk, ySk)
##         nCk_given_Sk        <- na_b(nCk, structure(Sk_idx_in_Ck, names = ySk) )
##         p_nCk_given_Sk_ySk  <- nCk_given_Sk / nSk_ySk # Cant be Inf, since ySk MUST be present since we simulated it
##         y_sim_z[Ck_idx_minus_Sk_idx] <- .split_chars(sample(names( p_nCk_given_Sk_ySk), 1L, prob =  p_nCk_given_Sk_ySk))
##       }
##     }
##     TY(structure(y_sim_z, names = Delta), C_marginals, S_marginals)    
##   })
##   doParallel::stopImplicitCluster()
##   y
## }


## #' Outlier tests in decomposable graphical models
## #' 
## #' @export
## outlier_model <- function(df,
##                           component_list     = NULL,
##                           nsim               = 1000,
##                           ncores             = 1,
##                           g                  = NULL,
##                           moment1            = TRUE,
##                           moment2            = FALSE,
##                           trace              = FALSE,
##                           meta_name          = "") {
##   ## INPUT:
##   ## -------
##   ## component_list : A NAMED list of components with each element being a character of
##   ## g              : An (igraph) interaction graph supplied by the user
##   ## meta_name      : Just a name for keeping track of the meta databse under H0.
##   ## moments        : should the theoretical mean(1) and variance(2) be calculated

##   ## OUTPUT:
##   ## -------
##   ## An outlier_model from which outlier tests can be conducted.

##   ## Comments:
##   ## ---------
##   ## It is ASSUMED that all values _for all variables_ in df are represented as a single character
##   ## - Maybe we will implement a conversion, e.g. levels(df$x1) <- letters[1:length(levels(df$x1))]
##   if( !setequal(colnames(df), unname(unlist(component_list))) ) stop("variables in df and component_list do not agree")
##   G     <- if( !is.null(g) ) g else Reduce(igraph::graph.union, efs_graph(df, component_list))
##   G_A   <- igraph::as_adj(G)
##   rip   <- rip(G_A)
##   C     <- rip$C
##   S     <- rip$S
##   Cms   <- a_marginals(df, C)
##   Sms   <- a_marginals(df, S)
##   sims  <- simulate_TY(df, Cms, Sms, nsim = nsim, ncores = ncores , verbose = verbose)
##   mu    <- NA
##   sigma <- NA
##   # if( moment1 ) mu <- E_TY(df, C_marginals, S_marginals)
##   # if( moment1 && moment2 ) sigma <- V_TY(df, C_marginals, S_marginals, mu)
##   mu_hat    <- mean(sims)
##   sigma_hat <- var(sims)
##   cdf       <- ecdf(sims)
##   out <- structure(class = "outlier_model",
##     list(
##       df          = df,
##       meta_name   = meta_name,
##       G           = G,
##       sims        = sims,
##       mu          = mu,
##       sigma       = sigma,
##       mu_hat      = mu_hat,
##       sigma_hat   = sigma_hat,
##       cdf         = cdf,
##       C_marginals = Cms,
##       S_marginals = Sms
##     )
##   )
##   return(out)
## }

## print.outlier_model <- function(x, ...) {
##   cat("\n Meta: ", x$meta_name,
##     "\n", paste(rep("-", 16), collapse = ""),
##     "",
##     paste(rep("-", nchar(x$meta_name)), collapse = ""),
##     "\n  Simulations:",         length(x$sims),
##     "\n  Components:",          components(x$G)$no,
##     "\n  Variables:",           ncol(x$df),
##     "\n  Observations:",        nrow(x$df),
##     "\n  Theoretical mean:",    round(x$mu, 2),
##     "\n  Theoretical variance:",round(x$sigma, 2),
##     "\n  Estimated mean:",      round(x$mu_hat, 2),
##     "\n  Estimated variance:",  round(x$sigma_hat, 2),
##     "\n  <outlier_model>", 
##     "\n ---------------",
##     "\n\n"
##   )
## }

## pmf <- function(x, ...) {
##   UseMethod("pmf")
## }

## pmf.outlier_model <- function(x, ...) {
##   hist(x$sims, breaks = 30, xlab = "T(y)", main = x$meta_name, freq = FALSE)
## }

## cdf <- function(x, ...) {
##   UseMethod("cdf")
## }

## cdf.outlier_model <- function(x, ...) {
##   x$cdf
## } 

## p_value <- function(x, ty_new, ...) {
##   UseMethod("p_value")
## }

## p_value.outlier_model <- function(x, ty_new, ...) {
##   1 - x$cdf( ty_new )
## }

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

## ## joint_component_prob <- function(x, y, k) {
## ##  UseMethod("joint_component_prob")
## ## }

## ## joint_component_prob.outlier_model <- function(x, y, k = seq_along(x$C_marginals)) {
## ##   ## For further marginal probabilities we need to propagate
## ##   ## k: integer or character specifying which components to obtain probabilities from
## ##   ## y: the profile
## ##   if( anyNA(y) ) stop( "y has NAs" )
## ##   df <- x$df
## ##   ps <- sapply(k, function(j) {
## ##     p_y(df, y, x$C_marginals[[j]], x$S_marginals[[j]] )
## ##   })
## ##   prod(ps)
## ## }

## `+.outlier_model` = function(e1, e2) {
##   # Convolution of two homogeneus outlier_models (same graphs, marginals etc.)
##   meta_name <- e1$meta_name
##   G         <- e1$G
##   sims      <- e1$sims + e2$sims
##   mu        <- e1$mu + e2$mu
##   sigma     <- e1$sigma + e2$sigma
##   mu_hat    <- e1$mu_hat + e2$mu_hat
##   sigma_hat <- e1$sigma_hat + e2$sigma_hat
##   cdf       <- ecdf(sims)
##   Cms       <- e1$C_marginals
##   Sms       <- e1$S_marginals
##   structure(class = "outlier_model",
##     list(
##       df          = e1$df,
##       meta_name   = meta_name,
##       G           = G,
##       sims        = sims,
##       mu          = mu,
##       sigma       = sigma,
##       mu_hat      = mu_hat,
##       sigma_hat   = sigma_hat,
##       cdf         = cdf,
##       C_marginals = Cms,
##       S_marginals = Sms
##     )
##   )
## }
