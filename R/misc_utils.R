#' Sparse table
#'
#' Returns a sparse contingency table for the variables in \code{x} as a vector .
#'
#' @param x matrix
#' @seealso \code{\link{csptable}}
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x))
  sptab <- n_a(x)
  class(sptab) <- c("sptable", class(sptab))
  sptab
}

#' Print sptable 
#'
#' A print method for \code{sptable} objects
#'
#' @param x A \code{sptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.sptable <- function(x, ...) {
  vars  <- attr(x, "vars")
  nchr  <- sum(sapply(vars, function(s) nchar(s))) + length(vars) - 1
  N     <- length(x)
  cells <- names(x)
  cat(paste0(vars, collapse = "-"), "\n")
  cat(paste0(rep("-", nchr), collapse = ""), "\n")
  for( i in 1:N ) {
    cat(paste0(cells[i], " : ", x[i]),"\n")
  }
}

#' Conditional sparse table
#'
#' Returns a conditional sparse contingency table (also called a slice) for the variables in \code{x} as a vector.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param b A named vector of indicies for which the variables are fixed
#' @description The names of \code{b} are the fixed values of the variables corresponding to the indicies
#' @seealso \code{\link{sptable}}
#' @export
csptable <- function(x, b) {
  stopifnot( "sptable" %in% class(x))
  csptab <- n_b(x, b)
  class(csptab) <- class(x)
  csptab
}

#' Print csptable 
#'
#' A print method for \code{csptable} objects
#'
#' @param x A \code{csptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.csptable <- function(x, ...) {
  ## TODO: Print the conditional information
  print.sptable(x, ...)
}

#' Converts and an adjacency matrix to an adjacency list
#'
#' @param A Adjacency matrix
#' @export
as_adj_lst <- function(A) {
  Delta <- colnames(A)
  out <- lapply(seq_along(Delta), function(r) {
    Delta[as.logical(A[, r])] # FIX!!! in efs_init matrices should be BOOLEAN!!!
  })
  names(out) <- Delta
  out
}

#' Converts and adjacency list to an adjacency matrix
#'
#' @param adj Adjacency list
#' @export
as_adj_mat <- function(adj) {
  Delta <- names(adj)
  N     <- length(Delta)
  A     <- matrix(0L, nrow = N, ncol = N, dimnames = list(Delta, Delta))
  for( d in seq_along(Delta) ) {
    idx <- match(adj[[d]], Delta)
    A[idx, d] <- 1L
  }
  A
}

#' A test for decomposability in undirected graphs
#'
#' This function returns \code{TRUE} if the graph is decomposable and \code{FALSE} otherwise
#'
#' @param adj Adjacency list of an undirected graph
#' @export
is_decomposable <- function(adj) {
  m <- try(mcs(adj), silent = TRUE)
  if( class(m) == "list" ) return(TRUE)
    else return(FALSE)
}

## examples:
## 4-cycle:
## adj <- list(a = c("b", "d"), b = c("a", "c"), c = c("b", "d"), d = c("a", "c"))
## is_decomposable(adj) # FALSE
## Two triangles:
## adj <- list(a = c("b", "d"), b = c("a", "c", "d"), c = c("b", "d"), d = c("a", "c", "b"))
## is_decomposable(adj) # TRUE
