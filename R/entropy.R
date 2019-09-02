na_tab <- function(df, a) {
  ct <- table(df[, a])
  names(dimnames(ct)) <- a # Needed for the onedimensional separators
  ct
}

joint_entropy <- function(df) {
  ## if( class(df) == "character" ) stop( "From entropy function: df is not a data.frame!" )
  x  <- na_tab(df, colnames(df))
  browser()
  Nx <- sum(x)
  entropy_table <- apply(x, seq_along(dim(x)), function(y) {
    ifelse(y == 0 , 0, y/Nx * log(y/Nx) )
  })
  -sum(entropy_table)
}

joint_entropy2 <- function(df) {
  A  <- apply(df, 1, paste0, collapse = "")
  x  <- table(A)
  Nx <- sum(x)
  -sum(x/Nx * log(x/Nx))
}

#' Joint Entropy
#' 
#' @description Calculates the joint entropy over discrete variables in \code{df}
#' 
#' @param df data.frame
#' @param thres A threshold mechanism for choosing between two different ways of calculating the entropy. Can Speed up the procedure with the "correct" value.
#' @return A numeric
#' @examples
#' # Joint entropy over five variables in tgp_dat
#' entropy(tgp_dat[, 5:9])
#' 
#' @export
entropy <- function(df, thres = 5) {
  browser()
  if( ncol(df) <= thres ) return(joint_entropy(df))
  else return(joint_entropy2(df))
}

entropy_difference <- function(e, S, df, ht, thres = 5) {
  v <- unlist(es_to_vs(e))

  H_S <- 0L        
  if (neq_empt_chr(S) ) {
    S_ <- sort_(S)
    if( exists(S_, envir = ht, inherits = FALSE) ) {
      H_S <- ht[[S_]]
    } else {
      H_S  <- entropy(df[S], thres)
      ht[[S_]] <- H_S
    }
  }

  H_S_x <- 0L        
  Sx <- sort_(c(S, v[1]))
  if( exists(Sx, envir = ht, inherits = FALSE) ) {
    H_S_x <- ht[[Sx]]
  } else {
    H_S_x  <- entropy(df[c(S, v[1])], thres)
    ht[[Sx]] <- H_S_x 
  }
  
  H_S_y <- 0L
  Sy <- sort_(c(S, v[2]))
  if( exists(Sy, envir = ht, inherits = FALSE) ) {
    H_S_y <- ht[[Sy]]
  } else {
    H_S_y  <- entropy(df[c(S, v[2])], thres)
    ht[[Sy]] <- H_S_y 
  }
  
  H_S_xy <- 0L
  Sxy <- sort_(c(S, v))
  if( exists(Sxy, envir = ht, inherits = FALSE) ) {
    H_S_xy <- ht[[Sxy]]
  } else {
    H_S_xy  <- entropy(df[c(S, v)], thres)
    ht[[Sxy]] <- H_S_xy  
  }
  
  H_S_x_S_y <- H_S_x + H_S_y
  # Test needed to avoid < 0 due to floating point errors
  edge_ent <- ifelse( isTRUE(all.equal(H_S_x_S_y, H_S_xy)), 0L,  H_S_x_S_y - H_S_xy - H_S )
  return( list(ent = edge_ent, ht = ht ))
}

