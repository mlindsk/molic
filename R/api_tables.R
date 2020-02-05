#' Sparse table
#'
#' Returns a sparse contingency table for the variables in \code{x} as a vector .
#'
#' @param x Character matrix
#' @seealso \code{\link{csptable}}
#' @examples
#' sptable(as.matrix(digits[, 1:2]))
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x))
  sptab <- n_a(x)
  class(sptab) <- c("sptable", class(sptab))
  sptab
}

#' Conditional sparse table
#'
#' Returns a conditional sparse contingency table (also called a slice) for the variables in \code{x} as a vector.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param b A named vector of indicies for which the variables are fixed
#' @description The names of \code{b} are the fixed values of the variables corresponding to the indicies
#' @seealso \code{\link{sptable}}
#' @examples
#' sp <- sptable(as.matrix(digits[, 1:3]))
#' sp
#' y  <- structure(2, names = "a")
#' csptable(sp, y)
#' @export
csptable <- function(x, b) {
  stopifnot( "sptable" %in% class(x))
  csptab <- n_b(x, b)
  class(csptab) <- c("csptable", class(x))
  csptab
}

#' Print table
#'
#' A print method for \code{sptable} and \code{csptable} objects
#'
#' @param x A \code{sptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.sptable <- function(x, ...) {
  vars  <- attr(x, "vars")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) - 1
  N     <- length(x)
  cells <- names(x)
  cat(paste0(vars, collapse = "-"), "\n")
  cat(paste0(rep("-", nchr), collapse = ""), "\n")
  for( i in 1:N ) {
    cat(paste0(cells[i], " : ", x[i]),"\n")
  }
}
