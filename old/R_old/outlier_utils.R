## .expand_grid <- function(...) expand.grid(..., stringsAsFactors = FALSE)

log_lik_pval <- function(x) {
  # x : 2-dimensional array (matrix)
  if(!is.matrix(x)) x <- as.matrix(x)
  # helpers:
  ps <- function(n, m) if(m == 0) return(0) else return(n / m)
  npl <- function(n, p) sum( ifelse(p == 0, 0, n*log(p)) )
  rr <- nrow(x); cc <- ncol(x); M  <- sum(x)
  n_row <- apply(x, 1, sum); n_col <- apply(x, 2, sum); n_x <- c(x)
  p_row <- ps(n_row, M); p_col <- ps(n_col, M); p_x <- ps(n_x, M)
  G <- -2*( npl(n_row, p_row) + npl(n_col, p_col) - npl(n_x, p_x) )
  1 - pchisq(G, (rr - 1) * (cc - 1))
}

weight_matrix <- function(df) {
  I       <- colnames(df)
  A       <- matrix(0L, length(I), length(I), dimnames = list(I, I))
  if( length(I) == 1 ) return(A) # For independence models
  mt_comb <-  combn(I, 2, simplify = FALSE)
  lapply(mt_comb, function(i) {
    comb    <- i
    df_p    <- df[, comb]
    ct_comb <- table(df_p)
    pval    <- log_lik_pval(ct_comb)
    A[comb[1], comb[2]] <<- pval + 1 # These + 1's assures a complete graph
    A[comb[2], comb[1]] <<- pval + 1 # which we want before we find a spanning tree
    NULL
  })
  A
}

na <- function(df, a) {
  # The a- marginal table
  if( !neq_empt_chr(a) ) return(nrow(df)) # vars = NULL
  # Keep "a" as an attribute for lookup possibility
  structure(table(apply(df[a], 1, paste0, collapse = "")), vars = a)
}

.split_chars <- function(x) unlist(strsplit(x, ""))

na_b <- function(na, b) {
  # The b'th slice in the a-marginal table
  # - b is a __NAMED__ vector of indexes pointing to the positions of the b levels
  # - ATCGTT  b = (ACG)(1,3,4)
  if( !neq_empt_num(b) ) stop("b is empty")
  cells    <- names(na)
  vars     <- attr(na, "vars")
  .nc      <- length(vars)
  fix_b    <- rep(".", .nc)
  fix_b[b] <- names(b)
  fix_b    <- paste0(fix_b, collapse = "")
  .na_b    <- na[grepl(fix_b, cells)]
  N        <- names(.na_b) 
  names(.na_b) <- sapply(N, function(f) paste0(.split_chars(f)[-b], collapse = ""))
  attr(.na_b, "vars") <- vars[-b]
  .na_b
}

na_ya <- function(na, ya) {
  # Counts in cell ya in a-marginal table
  val <- unname(na[paste0(ya, collapse = "")]) 
  if(is.na(val)) return(0L) else return(val)
}

a_marginals <- function(df, am) {
  # INPUT:
  # am: A list with all cliques / separators retrived from rip.R
  # OUTPUT:
  # A list with all marginal contingency tables
  lapply( am, function(x) if(is.null(x)) return(x)  else return(na(df, x)) )  
}

xlogx <- function(x) { # G
  x_ <- x > 0L
  x[x_] <- x[x_]*log(x[x_])
  x[!x_] <- 0L
  x
}

x1logx1 <- function(x) xlogx(x-1) - xlogx(x) # H

TY <- function(y, C_marginals, S_marginals) {
  # y: _named_ vector/observation
  Cs <- sapply(C_marginals, function(x) {
    na_ya(x, paste0(y[attr(x, "vars")], collapse = ""))
  })
  Ss <- if( length(S_marginals) >1) sapply(S_marginals[-1], function(x) {
    na_ya(x, paste0(y[attr(x, "vars")], collapse = ""))
  }) else 0L
  sum(x1logx1(Cs) - c(0L, x1logx1(Ss))) # H(C_yk) - H(S_yk)
}


## LET df BE A MATRIX WITH DIMNAMES = list(NULL, c(colnames))
simulate_TY <- function(df,
                  C_marginals,
                  S_marginals,
                  nsim            = 1000,
                  ncores          = 1,
                  verbose         = TRUE) {
  # OUTPUT: Simulated TY values of cells from the database df
  y <- replicate(nsim, vector("character", ncol(df)), simplify = FALSE)
  M <- nrow(df)
  Delta   <- colnames(df)
  C1_vars <- attr(C_marginals[[1]], "vars")
  C1_idx  <- match(C1_vars, Delta)
  p_nC1   <- C_marginals[[1]] / M
  yC1_sim <- sample(names(p_nC1), nsim, replace = TRUE, prob = p_nC1)
  if(!( length(C_marginals) - 1L)) {
    # The complete graph
    yC1_sim <- lapply(strsplit(yC1_sim, ""), function(z) {names(z) = C1_vars; z})
    return( sapply(yC1_sim, TY, C_marginals, S_marginals) )
  } 
  doParallel::registerDoParallel(ncores)
  y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim, .combine = 'c'), {
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
        nSk_ySk             <- na_ya(nSk, ySk)
        nCk_given_Sk        <- na_b(nCk, structure(Sk_idx_in_Ck, names = ySk) )
        p_nCk_given_Sk_ySk  <- nCk_given_Sk / nSk_ySk # Cant be Inf, since ySk MUST be present since we simulated it
        y_sim_z[Ck_idx_minus_Sk_idx] <- .split_chars(sample(names( p_nCk_given_Sk_ySk), 1L, prob =  p_nCk_given_Sk_ySk))
      }
    }
    TY(structure(y_sim_z, names = Delta), C_marginals, S_marginals)    
  })
  doParallel::stopImplicitCluster()
  y
}

simulate_Y <- function(df,
                  C_marginals,
                  S_marginals,
                  nsim            = 1000,
                  ncores          = 1,
                  verbose         = TRUE) {
  # OUTPUT: Simulated TY values of cells from the database df
  y <- replicate(nsim, vector("character", ncol(df)), simplify = FALSE)
  M <- nrow(df)
  Delta   <- colnames(df)
  C1_vars <- attr(C_marginals[[1]], "vars")
  C1_idx  <- match(C1_vars, Delta)
  p_nC1   <- C_marginals[[1]] / M
  yC1_sim <- sample(names(p_nC1), nsim, replace = TRUE, prob = p_nC1)
  if(!( length(C_marginals) - 1L)) {
    # The complete graph
    return(yC1_sim)
  } 
  doParallel::registerDoParallel(ncores)
  y <- foreach::`%dopar%`(foreach::foreach(z = 1:nsim), {
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
        nSk_ySk             <- na_ya(nSk, ySk)
        nCk_given_Sk        <- na_b(nCk, structure(Sk_idx_in_Ck, names = ySk) )
        p_nCk_given_Sk_ySk  <- nCk_given_Sk / nSk_ySk # Cant be Inf, since ySk MUST be present since we simulated it
        y_sim_z[Ck_idx_minus_Sk_idx] <- .split_chars(sample(names( p_nCk_given_Sk_ySk), 1L, prob =  p_nCk_given_Sk_ySk))
      }
    }
    y_sim_z
  })
  doParallel::stopImplicitCluster()
  y
}
