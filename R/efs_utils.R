## -----------------------------------------------------------------------------
##                              TODO
## -----------------------------------------------------------------------------
# - edges_to_delete : convert to cpp
# - is_Cx           : convert to cpp
# - is_Ca_or_Cb     : convert to cpp
# - is_Ca_and_Cb    : convert to cpp
# ------------------------------------------------------------------------------


## -----------------------------------------------------------------------------
##                        SMALL HELPER FUNCTIONS
## -----------------------------------------------------------------------------
neq_null     <- function(x) !is.null(x)
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
es_to_vs     <- function(e) strsplit(e, "\\|")
vs_to_es     <- function(e) lapply(e, paste0, collapse = "|")
rev_es       <- function(e) sapply(es_to_vs(e), function(x) paste0(rev(x), collapse = "|"))
sort_        <- function(x) paste0(sort(x), collapse = "|")
is_Cx        <- function(m, Cx) sapply(m, function(x) setequal(x$C1, Cx) || setequal(x$C2, Cx))
is_Ca_or_Cb  <- function(m, x, y) {  # m: msi object
  sapply(m, function(z) {
    is_CaCb <- setequal(z$C1, x) || setequal(z$C2, y)
    is_CbCa <- setequal(z$C1, y) || setequal(z$C2, x)
    is_CaCb || is_CbCa
  })
}
is_Ca_and_Cb  <- function(m, x, y) { # m: msi object
  sapply(m, function(z) {
    is_CaCb <- setequal(z$C1, x) && setequal(z$C2, y)
    is_CbCa <- setequal(z$C1, y) && setequal(z$C2, x)
    is_CaCb || is_CbCa
  })
}

## na <- function(df, a) {
##   ct <- table(df[, a])
##   names(dimnames(ct)) <- a # Needed for the onedimensional separators
##   ct
## }

as_adj_lst <- function(A) { # For the RIP function
  Delta <- colnames(A)
  out <- lapply(seq_along(Delta), function(r) {
    Delta[A[, r]]
  })
  names(out) <- Delta
  out
}

## as_adj_mat <- function(adj) { # For the RIP function
##   Delta <- names(adj)
##   # USE THE match function
##   # apply(A, 2, function(r) Delta[r])
## }

## -----------------------------------------------------------------------------
##                                  METRICS
## -----------------------------------------------------------------------------

## THIS SHOULD BE DEPRECATED
## entropy <- function(df) {
##   ## if( class(df) == "character" ) stop( "From entropy function: df is not a data.frame!" )
##   x  <- na(df, colnames(df))
##   Nx <- sum(x)
##   entropy_table <- apply(x, seq_along(dim(x)), function(y) {
##     ifelse(y == 0 , 0, y/Nx * log(y/Nx) )
##   })
##   -sum(entropy_table)
## }

entropy <- function(df) {
  A  <- apply(df, 1, paste0, collapse = "") ## SHOULD BE matpr!!!
  x  <- table(A)                            ## SHOUOD BE count_unique!!!
  Nx <- sum(x)
  -sum(x/Nx * log(x/Nx))
}

## metric <- function(m) {
##   # x : character
##   switch(m,
##     "entropy"  = entropy,
##     "entropy2" = entropy2,
##     "pval"     = NULL,
##     "etc..."   = NULL
##   )
## }

## -----------------------------------------------------------------------------
##                                 TESTING
## -----------------------------------------------------------------------------
## current_eligible_edges <- function(S) {
##   # Remove dublicates!
##   x <- unique(names(unlist(lapply(S, "[[", "e"))))
##   y <- rev_es(x)
##   setdiff(x,y)
## }

## is_all_eligibles_present <- function(G, A, S) {
##   # A : G_A
##   # S : msi$S
##   current_eligibles <- unique(names(unlist(lapply(S, "[[", "e"))))
##   current_eligibles <- c(current_eligibles, rev_es(current_eligibles))
##   current_edges     <- attributes(igraph::E(G))$vnames
##   current_edges     <- c(current_edges, rev_es(current_edges))
##   if( neq_empt_chr(intersect(current_eligibles, current_edges)) ) stop("Overlap")
##   A_lgc    <- A == 0L
##   vertices <- dimnames(A)[[1]]
##   all_edges_not_in_G <- unlist(unique(lapply( seq_along(vertices) , function(x) {
##     v <- A_lgc[vertices[x],]
##     to_v <- names(v[v == TRUE])
##     to_v <- to_v[-which(to_v == vertices[x])]
##     unname(sapply(to_v, function(y) paste(vertices[x], y, sep = "|") ))
##   })))
##   edges_maybe_not_captured_by_efs <- setdiff(all_edges_not_in_G, current_eligibles)
##   if( setequal(edges_maybe_not_captured_by_efs, character(0)) ) return( TRUE )
##   present <- sapply(edges_maybe_not_captured_by_efs, function(e) {
##     vs <- unlist(es_to_vs(e))
##     is_decomposable(igraph::add_edges(G, c(vs[1], vs[2])))
##   })
##   out <- !all(present)
##   if( !isTRUE(out) ) print(present)
##   return(out)
## }

## -----------------------------------------------------------------------------
##                             INITIALIZATION
## -----------------------------------------------------------------------------
efs_init <- function(df) {
  nodes <- colnames(df)
  n     <- length(nodes)
  G_A   <- Matrix::Matrix(0L, n, n, dimnames = list(nodes[1:n], nodes[1:n]))
  G     <- igraph::graph_from_adjacency_matrix(G_A, mode = "undirected")
  CG    <- as.list(nodes)
  CG_A  <- Matrix::Matrix(1L, n, n, dimnames = list(nodes[1:n], nodes[1:n]))
  diag(CG_A) <- 0L
  pairs     <- utils::combn(nodes, 2,  simplify = FALSE) ## USE C++ version here!
  max_dst   <- 0L
  max_edge  <- ""
  max_nodes <- 0L
  max_idx   <- 0L
  # https://www.r-bloggers.com/hash-table-performance-in-r-part-i/
  ht  <-  new.env(hash = TRUE) # Hash table with all entropy information - names NEED to be sorted!
  for( j in 1:n ) ht[[nodes[j]]] <- entropy(df[nodes[j]])
  msi_S <- lapply(seq_along(pairs), function(p) {
    x  <- pairs[[p]]
    edge_x <- sort_(x)
    ht[[edge_x]] <<- entropy(df[x])
    dst_x  <- ht[[x[1]]] + ht[[x[2]]] - ht[[edge_x]]
    if( dst_x > max_dst ) {
      max_dst   <<- dst_x
      max_edge  <<- edge_x
      max_nodes <<- x
      max_idx   <<- p
    }
    # Attach entropy to S!?
    list(S = character(0L), e = structure(dst_x, names = edge_x), C1 = x[1], C2 = x[2])
  })
  # max_ins <- match(max_nodes, CG) ## A better name might be "max_clique_CG_index"
  ## adj <- as_adj_lst(G_A)
  msi <- list(S = msi_S, max = list(e = max_edge, idx = max_idx, ins = match(max_nodes, CG)))
  out <- list(G = G, G_A = G_A, CG = CG, CG_A = CG_A, MSI = msi, ht = ht)
  class(out) <- c("efs")
  return(out)
}

## -----------------------------------------------------------------------------
##                  STOPPING CRITERIA ( MINIMUM DESCRIPTION LENGTH)
## -----------------------------------------------------------------------------

mdl1 <- function(adj, df, d = 3) {
  # adj: Adjacency list
  RIP    <- rip(adj)
  cliqs  <- RIP$C
  seps   <- RIP$S
  Nobs   <- nrow(df)
  Nvars   <- ncol(df)
  logNvars <- log(Nvars)
  DL_graph <- sum(sapply(cliqs, function(z) logNvars + length(z) * logNvars )) 
  DL_prob <- d * sum(sapply(seq_along(cliqs), function(i) {
    if( i == 1L ) return( length(cliqs[[i]]) - 1 )
    Ci <- cliqs[[i]]
    Si <- seps[[i]]
    Ci_Si <- setdiff(Ci, Si)
    length(Si) * (length(Ci_Si) - 1)
  }))
  HM_C <- sum(sapply(cliqs, function(z) {
    ## dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
    ## dst(df[z])
    entropy(df[z])
  }))
  HM_S <- 0L
  if( length(seps[-1]) ) {
    HM_S <- sum(sapply(seps[-1], function(z) {
      if( !neq_empt_chr(z)) return(0L)
      ## dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
      ## dst(df[z])
      entropy(df[z])
    }))    
  }
  DL_data <- Nobs * (HM_C - HM_S)
  return( log(DL_graph + DL_prob + DL_data) )
}

mdl2 <- function(adj, df, d = 3) {
  # adj: Adjacency list
  # lv: Levelvector
  lv = sapply(df, function(x) length(unique(x)))
  RIP    <- rip(adj)
  cliqs  <- RIP$C
  seps   <- RIP$S
  Nobs   <- nrow(df)
  Nvars   <- ncol(df)
  logNvars <- log(Nvars)
  DL_graph <- sum(sapply(cliqs, function(z) logNvars + length(z) * logNvars )) 
  ## DL_prob need to be corrected
  DL_prob <- d * sum(sapply(seq_along(cliqs), function(i) {
    if( i == 1L ) return( prod(lv[cliqs[[i]]]) - 1 )
    Ci <- cliqs[[i]]
    Si <- seps[[i]]
    Ci_Si <- setdiff(Ci, Si)
    prod(lv[Si]) * ( prod(lv[Ci_Si]) - 1)
  }))
  HM_C <- sum(sapply(cliqs, function(z) {
    ## dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
    ## dst(df[z])
    entropy(df[z])
  }))
  HM_S <- 0L
  if( length(seps[-1]) ) {
    HM_S <- sum(sapply(seps[-1], function(z) {
      if( !neq_empt_chr(z)) return(0L)
      ## dst  <- if( length(z) <= thres ) metric("entropy") else metric("entropy2")
      ## dst(df[z])
      entropy(df[z])
    }))    
  }
  DL_data <- Nobs * (HM_C - HM_S)
  return( log(DL_graph + DL_prob + DL_data) )
}

mdl_ <- function(type) {
  switch(type,
    "mdl1" = mdl1,
    "mdl2" = mdl2,
    "daic"  = delta_aic)
}

delta_aic <- function(x, level_vec) {
  # x : efs object
  n           <- length(level_vec) # ncol(df)
  complete    <- n * (n-1L) / 2L
  local_info  <- x$MSI$S[[x$MSI$max$idx]]
  e           <- local_info$e[x$MSI$max$e]
  S           <- local_info$S
  vs          <- es_to_vs(names(e))[[1]]
  HM_HM_prime <- unname(e)
  dev         <- -2*n*HM_HM_prime
  d_parms     <- prod(level_vec[vs] - 1) * prod(level_vec[S])
  d_aic       <- 2 * (dev + d_parms)
  return(d_aic)
}

## -----------------------------------------------------------------------------
##                           SUB-ROUTINES FOR efs_step
## -----------------------------------------------------------------------------
make_G_dbl_prime <- function(Sab, G_A) {
  keepers <- setdiff(dimnames(G_A)[[1]], Sab)
  igraph::graph_from_adjacency_matrix(G_A[keepers, keepers], mode = "undirected")
}

edges_to_delete <- function(prone_to_deletion, TVL, MSab, Cab, Sab, cta, ctb) {
  ## TVL  : Temporary vertex list
  ## MSab : logical indicating which prone_to_deletion (those with C1 \cap C2 = Sab) that has eab = (va, vb)
  etd <- sapply(seq_along(MSab), function(k) {
    delete <- FALSE
    x <- prone_to_deletion[[k]]
    if( MSab[k] ) {
      if( !all(x$C1 %in% Cab) && !all(x$C2 %in% Cab) ) {
        delete <- TRUE
        TVL <- c(TVL, x$C1, x$C2)
      }
    }
    else {
      C1_minus_Sab <- setdiff(x$C1, Sab)
      C2_minus_Sab <- setdiff(x$C2, Sab)
      va_in_C1_Sab <- all(C1_minus_Sab %in% cta)
      va_in_C2_Sab <- all(C2_minus_Sab %in% cta)
      vb_in_C1_Sab <- all(C1_minus_Sab %in% ctb)
      vb_in_C2_Sab <- all(C2_minus_Sab %in% ctb)
      delete <- FALSE
      if( (va_in_C1_Sab && vb_in_C2_Sab) || (va_in_C2_Sab && vb_in_C1_Sab) ) {
        delete <- TRUE
      }
    }
    delete
  })
  list(del = Filter(neq_null, prone_to_deletion[etd]), TVL = TVL)
}

which_Cp_from_Cx_to_Cab <- function(CG_prime, C_prime_Cx, Cx, vx, Cab, Sab,  cty, TVL) {
  add <- vector("numeric", 0L)
  for( k in C_prime_Cx ) {
    Cp    <- CG_prime[[k]]
    Sp    <- intersect(Cp, Cx)
    Sn    <- intersect(Cp, Cab)
    Sab_x <- c(Sab, vx)
    if( neq_empt_chr(Sab) ) {
      if( all(Sp %in% Sab_x) ) { # NOT A PROPER SUBSET!!??
        add <- c(add, k)
      }
      if( setequal(Sn, Sab_x) && !(any(setdiff(Cp, Sab_x) %in% cty)) ) {
        add <- c(add, k)
      }
    }
    else {
      if( vx %in% Sn || !neq_empt_chr(Sp) ) {
        add <- c(add, k)
      }
    }
  }
  add_tvl <- which(CG_prime %in% TVL) # For all C' in TVL add (C', Cab) to CG_prime
  list(add = unique(add), add_tvl = unique(add_tvl)) 
}

update_edges_from_C_primes_to_Cab <- function(df, Cps, Cab, va, vb, ht) {
  # Cps : C_primes
  sep <- lapply(Cps, function(Cp) {
    Sp          <- intersect(Cp, Cab)
    eligs_Cab   <- setdiff(Cab, Cp)
    eligs_Cp    <- setdiff(Cp, Cab)
    eligs       <- expand.grid(eligs_Cp, eligs_Cab)
    eligs       <- apply(eligs, 1, paste, collapse = "|")
    eligs_names <- eligs
    ## dst  <- if( length(Sp) <= thres ) metric("entropy") else metric("entropy2")
    H_Sp <- 0L
    if( neq_empt_chr(Sp) ) H_Sp <- ht[[sort_(Sp)]]
    eligs  <- sapply(eligs, function(e) {
      ## See the proof of Theorem 4.3 in Jordan to optimize! (Dont need to use exists for all cases)
      v <- unlist(es_to_vs(e))
      H_Sp_x <- 0L        
      Spx <- sort_(c(Sp, v[1]))
      if( exists(Spx, envir = ht) ) {
        H_Sp_x <- ht[[Spx]]
      } else {
        H_Sp_x  <- entropy(df[c(Sp, v[1])]) ## dst(df[c(Sp, v[1])])
        ht[[Spx]] <<- H_Sp_x 
      }
      H_Sp_y <- 0L
      Spy <- sort_(c(Sp, v[2]))
      if( exists(Spy, envir = ht) ) {
        H_Sp_y <- ht[[Spy]]
      } else {
        H_Sp_y  <- entropy(df[c(Sp, v[2])]) ## dst(df[c(Sp, v[2])])
        ht[[Spy]] <<- H_Sp_y 
      }
      H_Sp_x_y <- 0L
      Spxy <- sort_(c(Sp, v))
      if( exists(Spxy, envir = ht) ) {
        H_Sp_xy <- ht[[Spxy]]
      } else {
        H_Sp_xy  <- entropy(df[c(Sp, v)]) ## dst(df[c(Sp, v)])
        ht[[Spxy]] <<- H_Sp_xy  
      }
      return( H_Sp_x + H_Sp_y - H_Sp_xy - H_Sp )
    })
    names(eligs) <- eligs_names
    list(S = Sp, e = eligs, C1 = Cp, C2 = Cab)
  })
  return(list(Filter(neq_null, sep), ht))
}

## -----------------------------------------------------------------------------
##                              THE ENGINE
## -----------------------------------------------------------------------------
efs_step <- function(df, x) {
  ## -----------------------------------------------------------------------------
  ##                    STORE ALL CURRENT INFORMATION
  ## -----------------------------------------------------------------------------
  ## x : efs object
  MSI <- x$MSI
  ht  <- x$ht
  msi <- MSI$S  
  mab <- MSI$max
  eab <- mab$e
  vab <- unlist(strsplit(eab, "\\|"))
  va  <- vab[1]
  vb  <- vab[2]
  Ca  <- msi[[mab$idx]]$C1
  Cb  <- msi[[mab$idx]]$C2
  Sab <- msi[[mab$idx]]$S
  Cab <- c(Sab, va, vb)

  G_prime     <- x$G
  G_prime_A   <- x$G_A
  G_prime_A[va, vb] <- 1L # Adding the new edge (va, vb)
  G_prime_A[vb, va] <- 1L
  G_prime     <- igraph::add_edges(G_prime, c(va, vb))
  CG_prime    <- x$CG
  CG_prime_A  <- x$CG_A
  G_dbl_prime <- make_G_dbl_prime(Sab, x$G_A)
  msi_prime   <- msi
  
  ## Vertices connected to a and b in G_dbl_prime
  cta <- names(as.list(igraph::bfs(G_dbl_prime, va, unreachable = FALSE)$order))
  cta <- cta[!is.na(cta)]
  ctb <- names(as.list(igraph::bfs(G_dbl_prime, vb, unreachable = FALSE)$order))
  ctb <- ctb[!is.na(ctb)]
  
  ## -----------------------------------------------------------------------------
  ##       INSERTING Cab BETWEEN Ca AND Cb IN CG_prime AND REMOVE (Ca, Cb)
  ## -----------------------------------------------------------------------------
  ## Add Cab to CG_prime and add the edges (Ca, Cab) and (Cb, Cab) to CG_prime_A
  ins          <- vector("numeric", nrow(CG_prime_A))
  ins[mab$ins] <- 1L
  CG_prime_A   <- rbind(CG_prime_A, ins)
  CG_prime_A   <- cbind(CG_prime_A, c(ins, NA)) # NA since no loops
  rownames(CG_prime_A) <- NULL
  colnames(CG_prime_A) <- NULL
  CG_prime[[length(CG_prime) + 1L]]   <- Cab

  ## Delete (Ca, Cb) in CG_prime_A
  CG_prime_A[matrix(mab$ins, 1)]      <- 0L
  CG_prime_A[matrix(rev(mab$ins), 1)] <- 0L

  ## -----------------------------------------------------------------------------
  ##                  DELETING EDGES FROM CG IN CG_prime
  ## -----------------------------------------------------------------------------
  TVL  <- vector("list", 0L) # Temporary Vertex List (see Altmueller)
  Sabs <- sapply(msi, function(s) setequal(s$S, Sab)) # msi corresponds to CG
  prone_to_deletion <- msi[Sabs]
  MSab <- sapply(prone_to_deletion, function(x) { # See Altmueller
    es <- names(x$e)
    ## --------
    if( eab %in% es) {
      return(TRUE)
    } else if(rev_es(eab) %in% es ) { # Bottleneck
      return(TRUE)
    } else {
      return(FALSE)
    }
  })

  etd <- edges_to_delete(prone_to_deletion, TVL, MSab, Cab, Sab, cta, ctb) 
  delete_edges <- etd$del
  TVL <- etd$TVL

  if( neq_empt_lst(delete_edges) ) {
    delete_idx <- lapply(delete_edges, function(x) {
      sapply(seq_along(CG_prime), function(k) {
        y <- CG_prime[[k]]
        ifelse(setequal(x$C1, y) || setequal(x$C2, y), k, NA)
      })
    })
    for( i in delete_idx ) {
      k <- stats::na.omit(i)
      CG_prime_A[k[1], k[2]] <- 0L
      CG_prime_A[k[2], k[1]] <- 0L
    }
    ## Step 3.3.2 in Jordan
    msi_prime[which(msi_prime %in% delete_edges)] <- NULL
  }

  ## -----------------------------------------------------------------------------
  ##                       ADDING EDGES TO CG_prime
  ## -----------------------------------------------------------------------------
  ## Prime clique indicies
  Cp_idx     <- CG_prime_A[mab$ins,]
  C_prime_Ca <- which(Cp_idx[1, , drop = TRUE] == 1L)
  C_prime_Ca <- C_prime_Ca[-length(C_prime_Ca)]
  C_prime_Cb <- which(Cp_idx[2, , drop = TRUE] == 1L) 
  C_prime_Cb <- C_prime_Cb[-length(C_prime_Cb)]

  add_a   <- which_Cp_from_Cx_to_Cab(CG_prime, C_prime_Ca, Ca, va, Cab, Sab, ctb, TVL)
  add     <- c(add_a$add, add_a$add_tvl)
  add_b   <- which_Cp_from_Cx_to_Cab(CG_prime, C_prime_Cb, Cb, vb, Cab, Sab, cta, TVL)
  add     <- unique(c(add, add_b$add, add_b$add_tvl))

  if( neq_empt_num(add) ) {
    CG_prime_A[add, length(CG_prime)] <- 1L
    CG_prime_A[length(CG_prime), add] <- 1L
  }

  ## Needed to update msi_prime
  C_primes <- CG_prime[add]
  
  ## -----------------------------------------------------------------------------
  ##                       DELETE Ca AND Cb IF IN Cab
  ## -----------------------------------------------------------------------------
  Ca_in_Cab <- all(Ca %in% Cab)
  Cb_in_Cab <- all(Cb %in% Cab)
  Ca_Cb_idx <- mab$ins
  CG_Ca_idx <- mab$ins[1]
  CG_Cb_idx <- mab$ins[2]

  if( Ca_in_Cab || Cb_in_Cab ) {
    if( Ca_in_Cab && Cb_in_Cab ) {
      CG_prime     <- CG_prime[-Ca_Cb_idx]
      CG_prime_A   <- CG_prime_A[-Ca_Cb_idx, -Ca_Cb_idx]
      msi_prime    <- msi_prime[!is_Ca_or_Cb(msi_prime, Ca, Cb)]
    }
    else if( Ca_in_Cab ) {
      CG_prime     <- CG_prime[-CG_Ca_idx]
      CG_prime_A   <- CG_prime_A[-CG_Ca_idx, -CG_Ca_idx]
      msi_prime  <- msi_prime[!is_Cx(msi_prime, Ca)]
      ## We need to update C_primes to include Ca or Cb if they are not in Cab!!!
      C_primes <- c(C_primes, list(Cb)) # Since Cb not in Cab then
    } else {
      CG_prime     <- CG_prime[-CG_Cb_idx]
      CG_prime_A   <- CG_prime_A[-CG_Cb_idx, -CG_Cb_idx]
      msi_prime    <- msi_prime[!is_Cx(msi_prime, Cb)]
      ## We need to update C_primes to include Ca or Cb if they are not in Cab!!!
      C_primes  <- c(C_primes, list(Ca)) # Since Ca not in Cab then
    }
  } else {
    msi_prime <- msi_prime[!is_Ca_and_Cb(msi_prime, Ca, Cb)]
    C_primes <- c(C_primes, list(Ca), list(Cb)) # Since Ca and Cb not in Cab then
  }

  ## -----------------------------------------------------------------------------
  ##                       CALCULATE NEW ENTROPIES
  ## -----------------------------------------------------------------------------
  ue         <- update_edges_from_C_primes_to_Cab(df, C_primes, Cab, va, vb, ht)
  ht         <- ue[[2]]
  msi_prime  <- c(msi_prime, ue[[1]])
  if( !neq_empt_lst(msi_prime) ) { # If the graph is complete
    out <- list(G = G_prime,
      G_A         = G_prime_A,
      CG          = CG_prime,
      CG_A        = CG_prime_A,
      MSI         = msi_prime,
      ht          = ht
    )
    class(out) <- c("efs")
    return(out)
  }
  max_es     <- sapply(msi_prime, function(x) x$e[which.max(x$e)])
  wm_max_es  <- which.max(max_es)
  maxy       <- msi_prime[[wm_max_es]]
  e_max      <- max_es[wm_max_es]
  idx_max    <- wm_max_es
  ins_max    <-unlist(sapply(seq_along(CG_prime), function(x) {
    cond <- setequal(CG_prime[[x]], maxy$C1) || setequal(CG_prime[[x]], maxy$C2)
    if( cond ) return(x)
  }))
  MSI_prime <- list(S = msi_prime, max = list(e = names(e_max), idx = idx_max, ins = ins_max))
  out <- list(G = G_prime,
    G_A         = G_prime_A,
    CG          = CG_prime,
    CG_A        = CG_prime_A,
    MSI         = MSI_prime,
    ht          = ht
  )
  class(out) <- c("efs")
  return(out)
}

