.expand_grid <- function(...) expand.grid(..., stringsAsFactors = FALSE)

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
  if( !neq_empt_chr(a) ) stop("a is the empty set")
  # Keep "a" as an attribute for lookup possibility
  structure(table(apply(df[, a], 1, paste0, collapse = "")), vars = a)
}

na_ya <- function(na, ya) {
  # Counts in cell ya in a-marginal table
  val <- unname(na[paste0(ya, collapse = "")]) 
  if(is.na(val)) return(0L) else return(val)
}
  

na_b <- function(na, b) {
  # The b'th slice in the a marginal table
  # - b is a vector of strings (e.g. c("G", "T", "T"))
  # - the names of na are assumed to be sorted occording to
  # - ATCGTT where (ATCGTT) is the clique and (GTT) is the separator
  if( !neq_empt_chr(b) ) return(na)
  dimb  <- length(b)
  vals  <- names(na)
  nvals <- nchar(vals[1])
  avals <- substr(vals, 1L, nvals - dimb)
  bvals <- substr(vals, nvals - dimb + 1L, nvals)
  bs    <- which(bvals == paste0(b, collapse = ""))
  structure( na[bs], names = avals[bs], vars =  attr(na, "vars")[1:(nvals-dimb)])
}


a_marginals <- function(df, am) {
  # INPUT:
  # am: A list with all cliques / separators retrived from graphical_model.R
  # OUTPUT:
  # A list with all marginal contingency tables
  lapply( am, function(x) if(is.null(x)) return(x)  else return(na(df, x)) )  
}


## Let it output a function that can be used on y's
## p_y <- function(df, y, C_marginals, S_marginals) {
##   # INPUT:
##   # C_marginals: marginal clique tables from function a_marginals
##   # S_marginals: -
  
##   # OUTPUT:
##   # Joint density of Y over clique marginals

##   Delta   <- colnames(df) # Let it be an input?
##   M       <- nrow(df)
##   nC1_yC1 <- na_ya(Delta, C_marginals[[1]], y)
##   nC_nS   <- 1
##   if(length(C_marginals) - 1) {
##     nC_nS   <- vapply(X = 2:length(C_marginals), FUN.VALUE = 1, FUN =  function(k) {
##       nCk_yCk <- na_ya(Delta, C_marginals[[k]], y)
##       nSk_ySk <- na_ya(Delta, S_marginals[[k]], y)
##       ifelse(nSk_ySk > 0, nCk_yCk / nSk_ySk, 0)
##     })  
##   }
##   nC1_yC1 / M * prod(nC_nS)
## }

G_ <- function(x) x*log(x)

H_ <- function(x){
  G_(x-1) - G_(x)
} 

## T_Yk <- function(df, y, k, C_marginal_k, S_marginal_k){ ## DONT DRAG df AROUND!!!
##   # INPUT:
##   # a_marginal_k: the k'th marginal table from an a_marginals call

##   # OUTPUT:
##   # Tk evaluated at Yk = yk
##   Delta <- colnames(df) # Let it be an input?
##   H_Ck  <- H_(na_ya(Delta, C_marginal_k ,y))
##   if( k == 1 ) {
##     return( H_Ck )
##   } else {
##     H_Sk  <- H_(na_ya(Delta, S_marginal_k ,y))
##     return( H_Ck - H_Sk )
##   }
## }

## T_Yk2 <- function(df, yk, k, C_marginal_k, S_marginal_k){ ## DONT DRAG df AROUND!!!
##   # INPUT:
##   # a_marginal_k: the k'th marginal table from an a_marginals call
##   # yk: the marginal profile corresponding to the k'th marginal table(s)

##   # OUTPUT:
##   # Tk evaluated at Yk = yk
##   browser()

##   na_ya_k <- na_ya(C_marginal_k, paste0(yk, collapse = ""))

##   H_Ck  <- H_(na_ya(Delta, C_marginal_k ,y))

##   if( k == 1 ) {
##     return( H_Ck )
##   } else {
##     H_Sk  <- H_(na_ya(Delta, S_marginal_k ,y))
##     return( H_Ck - H_Sk )
##   }
## }

## -------------------------------------------------------------
## BOOKMARK!!!!                                                              
## -------------------------------------------------------------


TY <- function(y, C_marginals, S_marginals) {
  # y: named vector (observations)!!!
  Cs <- sapply(C_marginals, function(x) {
    na_ya(x, paste0(y[attr(x, "vars")], collapse = ""))
  })

  Ss <- sapply(S_marginals[-1], function(x) {
    na_ya(x, paste0(y[attr(x, "vars")], collapse = ""))
  })

  # Resort 0*log(0) in vectorisez function
  Ss <- c(0L, Ss)
  H_(Cs) - c(0, H_(Ss))
}


## -------------------------------------------------------------
## BOOKMARK                                                             
## -------------------------------------------------------------


T_Y <- function(df, y, C_marginals, S_marginals) { ## DONT DRAG df AROUND!!!
  # OUTPUT:
  # T evaluated at Y = y
  Tks <- vapply(X = seq_along(C_marginals), FUN.VALUE = 1, FUN =  function(k) {
    T_Yk(df, y, k, C_marginals[[k]], S_marginals[[k]])
  })
  sum(Tks)
}


E_TY <- function(df, C_marginals, S_marginals) {
  # OUTPUT:
  # Exptected value of T(Y)
  
  Delta  <- colnames(df)
  y      <- vector("character", length(Delta))
  M      <- nrow(df)

  E <- vapply(X = seq_along(C_marginals), FUN.VALUE = 1, FUN = function(k) {
    nSk       <- S_marginals[[k]]
    nCk       <- C_marginals[[k]]
    Ck_index  <- ia(Delta, names(dimnames(nCk)))
    Ck_levels <- dimnames(nCk)
    ICk   <- do.call(.expand_grid, Ck_levels)
    E_Tk  <- vapply(X = 1:nrow(ICk), FUN.VALUE = 1, FUN =  function(i) {
      y[Ck_index] <- unlist(ICk[i,])
      Tk <- T_Yk(df, y, k, nCk, nSk)
      Tk * na_ya(Delta, nCk, y) / M
    })
    sum(E_Tk)
  })
  sum(E)
}


V_TY <- function(df, C_marginals, S_marginals, E_ty) {
  # INPUT:
  # e_ty : A mean value obtained from E_TY

  # OUTPUT:
  # Variance of T(Y)
  Delta  <- colnames(df)
  I      <- do.call(.expand_grid, lapply(df, unique))
  nI     <- nrow(I)
  V      <- vector("numeric", length = nI)
  for( i in 1:nI ) {
    yi  <- unlist(I[i, ])
    Ty  <- T_Y(df, yi, C_marginals, S_marginals)
    V[i] <- (Ty - E_ty)^2 * p_y(df, yi, C_marginals, S_marginals)
  }
  sum(V)
}

sim_Y <- function(df,
                  C_marginals,
                  S_marginals,
                  n.sim = 1000,
                  ncores = 1,
                  verbose = TRUE) {

  # OUTPUT:
  # A simulated profile from the database df

  y <- replicate(n.sim, vector("character", ncol(df)), simplify = FALSE)
  M <- nrow(df)
  
  Delta     <- colnames(df)
  p_nC1     <- C_marginals[[1]] / M
  C1_index  <- ia(Delta, names(dimnames(p_nC1)))
  C1_levels <- dimnames(p_nC1)

  IC1     <- do.call(.expand_grid, C1_levels)
  yC1_sim <- sample(1:nrow(IC1), n.sim, replace = TRUE, prob = c(p_nC1))

  if(verbose) cat("\nSimulating... \n")
  if( !verbose ) pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(type = "timer"))
  
  y <- pbapply::pblapply(X = 1:n.sim, cl = ncores, FUN =  function(z) {
    y_sim_z <- y[[z]]
    y_sim_z[C1_index] <- unlist(IC1[yC1_sim[z],])

    # Handling components wich are complete (and thereby no separators)
    if( length(S_marginals) == 1 ) return(y_sim_z)

    for( k in 2:length(C_marginals) ) {
      nCk  <- C_marginals[[k]]
      nSk  <- S_marginals[[k]]
      Ck   <- names(dimnames(nCk)) # Clique names
      Sk   <- names(dimnames(nSk)) # Separator names

      Ck_index <- ia(Delta, names(dimnames(nCk)))
      Sk_index <- ia(Delta, names(dimnames(nSk)))

      ySk      <- y_sim_z[matrix(Sk_index, 1)]
      nSk_ySk  <- na_ya(Delta, nSk, y_sim_z)

      Ck_minus_Sk_levels  <- dimnames(nCk)[-which(Ck %in% Sk)]
      ICk_minus_Sk        <- do.call(.expand_grid, Ck_minus_Sk_levels)
      N_ick_minus_sk      <- 1:nrow(ICk_minus_Sk)
      
      indexC              <- which(!(Ck %in% Sk))
      nCk_given_nSk       <- apply(nCk, indexC, function(x) x[matrix(ySk, 1)])

      p_nCk_given_nSk     <- nCk_given_nSk / nSk_ySk ## Can this be zero? Make an ifelse(t, y, n)
      yCk_minus_ySk_sim_z <- sample(N_ick_minus_sk, 1, prob = p_nCk_given_nSk)
      Ck_minus_Sk_index   <- Ck_index[-which(Ck_index %in% Sk_index)]
      y_sim_z[Ck_minus_Sk_index] <- unlist(ICk_minus_Sk[yCk_minus_ySk_sim_z, ])
    }
    y_sim_z  
  })
  y
}

sim_TY <- function(df,
                   C_marginals,
                   S_marginals,
                   n.sim = 1000,
                   ncores = 1,
                   verbose = TRUE) {
  # OUTPUT:
  # Simulated values of T(Y) from the database df
  y_sim <- sim_Y(df,
    C_marginals,
    S_marginals,
    n.sim,
    ncores,
    verbose
  )

  if(verbose) cat("\nCalculating T(y)...\n")
  if(!verbose) pbapply::pboptions(type = "none")
  on.exit(pbapply::pboptions(type = "timer"))

  pbapply::pbsapply(X = y_sim,
    cl = ncores,
    FUN = function(y) T_Y(df, y, C_marginals,S_marginals)
  ) 
}
