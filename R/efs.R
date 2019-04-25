efs <- function(df, x = efs_init(df), d = 3, thres = 3, trace = TRUE, log_mdl = TRUE, method = "forward") {
  # INPUT:
  # df: dataframe
  # x: efs object. Default is the null-graph
  # d: number needed to encode a single paramter (see Altmueller)
  # thres: When the size of a set is larger than thres, entropy2 is used for speed
  # - For binary data the "optimal" value of thres is closer to 7
  n        <- ncol(df)
  complete <- n * (n-1L) / 2L
  k        <- length(igraph::E(x$G))
  if( k == complete ) stop("The graph is already complete!")
  prev_mdl <- mdl(x$G_A, df, d, thres)
  x        <- efs_step(df, x, thres)
  curr_mdl <- mdl(x$G_A, df, d, thres)
  k <- k + 1L
  if( curr_mdl > prev_mdl || k == complete) return(x)  
  while( curr_mdl <= prev_mdl ) {
    if( k == complete ) {
      if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(curr_mdl, 6L)), "\n")
      return(x)
    } 
    if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(curr_mdl, 6L)), "\n")
    x    <- efs_step(df, x, thres)
    k    <- k + 1L
    prev_mdl <- curr_mdl
    curr_mdl <- mdl(x$G_A, df, d, thres)
  }
  if( trace ) cat(paste(" Edges:", k, "of", complete, "- mdl =", round(prev_mdl, 6L)), "\n")
  return(x)
}

print.efs <- function(x, ...) {
  nv <- ncol(x$G_A)
  ne <- length(igraph::E(x$G))
  print(ne)
  cat(" A Decomposable Graph With",
    "\n -------------------------",
    "\n  Nodes:", nv,
    "\n  Edges:", ne, "/", nv*(nv-1)/2,
    "\n  Cliques:", length(x$CG),
    "\n  <efs>",
    "\n -------------------------\n"
  )
}

## mdl2 <- function(G, df, d = 3, thres = 7) {
##   level_vec <- vapply(df, function(z) {length(unique(z))}, 1L)
##   RIP    <- rip(G)
##   cliqs  <- RIP$C
##   seps   <- RIP$S
##   Nobs   <- nrow(df)
##   Nvars   <- ncol(df)
##   logNvars <- log(Nvars)

##   DL_graph <- sum(sapply(cliqs, function(z) logNvars + length(z) * logNvars )) 

##   DL_prob <- d * sum(sapply(seq_along(cliqs), function(i) {
##     browser()

##     if( i == 1L ) return( length(cliqs[[i]]) - 1 )
##     Ci <- cliqs[[i]]
##     Si <- seps[[i]]
##     Ci_Si <- setdiff(Ci, Si)
##     length(Si) * (length(Ci_Si) - 1)
##   }))

##   HM_C <- sum(sapply(cliqs, function(z) {
##     dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
##     dst(df[, z])
##   }))

##   HM_S <- 0L

##   if( length(seps[-1]) ) {
##     HM_S <- sum(sapply(seps[-1], function(z) {
##       if( !neq_empt_chr(z)) return(0L)
##       dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
##       dst(df[, z])
##     }))    
##   }

##   DL_data <- Nobs * (HM_C - HM_S)

##   return( log(DL_graph + DL_prob + DL_data) )
## }

## efs1 <- function(df, x = efs_init(df), thres = 3, trace = TRUE) {
##   level_vec <- vapply(df, function(z) {length(unique(z))}, 1L)
##   n        <- ncol(df)
##   complete <- n * (n-1L) / 2L
##   k        <- length(igraph::E(x$G))
##   if( k == complete ) stop("The graph is already complete!")

##   ## ---------------------- ##
##   ## WRAP IT IN A FUNCTION! ##
##   ## ---------------------- ##
##   local_info <- x$MSI$S[[x$MSI$max$idx]]
##   e  <- local_info$e
##   S  <- local_info$S
##   vs <- es_to_vs(names(e))[[1]]
##   # Deviance = -2 log Q - NO!!!! It is just the difference in entropy!
##   HM_HM_prime <- unname(e)
##   # Also Handles character(0)
##   delta_parms <- prod(level_vec[vs] - 1) * prod(level_vec[S])
##   delta_aic <- 2 * HM_HM_prime + 2 * delta_parms
##   k <- k + 1L
##   while( delta_aic < 10 ) {
##     x    <- efs_step(df, x, thres)
##     k    <- k + 1L
##     local_info <- x$MSI$S[[x$MSI$max$idx]]
##     e  <- local_info$e[x$MSI$max$e]
##     S  <- local_info$S
##     vs <- es_to_vs(names(e))[[1]]
##     HM_HM_prime <- unname(e)
##     delta_parms <- prod(level_vec[vs] - 1) * prod(level_vec[S])
##     delta_aic <- 2 * HM_HM_prime + 2 * delta_parms
##     print(delta_aic)
##     if( k == 500 ) stop("Many")
##   }
##   return(x)
## }





## source("../R/load_all.R")
## load("../data/tgp_dat.rda")
## load("../data/tgp_haps.rda")
## library(dplyr)
## library(igraph)
## haps <- tgp_haps[1:10]
## df   <- tgp_dat
## df   <- df %>% filter(pop_meta == "EUR") %>% select(unname(unlist(haps)))
## df


## y1 <- efs(df)
## y2 <- efs1(df)

## plot(y1$G)
## plot(y2$G)
