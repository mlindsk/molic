find_cond_configs <- function(x, pos) {
  # x  : sptable
  # pos: the position of the conditional variables
  skeleton <- paste(rep("@", nchar(names(x)[1])))
  .map_chr(names(x), function(s) {
    sk <- skeleton
    s_pos_val <- .map_chr(pos, function(l) substr(s, l, l))
    sk[pos] <- s_pos_val
    paste(gsub("@", "", sk), collapse = "")
  })
}

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

sum.sptable <- function(x, y) {
  # x : sptable
  # y : variables to sum out (the separator)
  
}

#' Slice of a sparse table
#'
#' Returns the b-slice of a conditional sparse contingency table for the variables in \code{x} as a vector.
#'
#' @param x A \code{sptable} object
#' @param b A named vector of indicies for which the variables are fixed
#' @description The names of \code{b} are the fixed values of the variables corresponding to the indicies
#' @seealso \code{\link{sptable}}
#' @examples
#' sp <- sptable(as.matrix(digits[, 1:3]))
#' # print(sp)
#' slice_sptable(sp, c(a = 2))
#' @export
slice_sptable <- function(x, b) {
  stopifnot(inherits(x, "sptable"))
  sl <- n_b(x, b)
  class(sl) <- c("slice", class(x))
  return(sl)
}

#' Conditional probability table
#'
#' Returns a sparse conditional probability table conditioned on variables in \code{b}.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param y Character vector with variables to condition on
#' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{sptable} with no conditioning variables, i.e. the marginal table.
#' @seealso \code{\link{sptable}}, \code{\link{slice_sptable}}
#' @examples
#' sp  <- sptable(as.matrix(digits[, 1:3]))
#' psp <- parray(sp, c("V1", "V2"))
#' sum(psp[1:length(letters[1:17])]) # V3 has 17 levels.
#' @export
parray <- function(x, y = NULL) {
  # x : sptable
  # y : conditional variables
  stopifnot(inherits(x, "sptable"))
  if (is.null(y)) {
    class(x) <- c("parray", class(x))
    return(x / sum(x))
  }
  pos  <- match(y, attr(x, "vars"))
  conf <- find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)
  parr <- unlist(unname(lapply(conditional_list, function(e) {
    x[e] / sum(x[e])
  })))
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- c("parray", class(x))
  parr
}

#' Conditional sparse table
#'
#' Returns a conditional sparse contingency table conditioned on variables in \code{b}.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param y Character vector with variables to condition on
#' @param parray Logical if the cell values should be converted to probabilities
#' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{csptable} with no conditioning variables, i.e. the marginal table.
#' @seealso \code{\link{sptable}}, \code{\link{slice_sptable}}
#' @examples
#' sp <- sptable(as.matrix(digits[, 1:3]))
#' sp
#' length(sp)
#' csp <- csptable(sp, c("V3"))
#' csp
#' length(csp)
#' @export
csptable <- function(x, y, parray = TRUE) {
  # x : sptable
  # y : conditional variables

  if (is.null(y)) {
    cspt <- structure(list(x), names = "marg")
    class(cspt) <- c("csptable", class(cspt))
    return(cspt)
  }
  # check if y is in the names of x
  if (!all(y %in% attr(x, "vars"))) stop("Some names in 'y' are not in 'x'")
  
  pos     <- match(y, attr(x, "vars"))
  configs <- find_cond_configs(x, pos)
  cspt <- lapply(configs, function(z) {
    b  <- structure(pos, names = .split_chars(z))
    sl <- slice_sptable(x, b)
    if (parray) return(sl / sum(sl))
    return(sl)
  })
  cspt <- structure(cspt, names = configs, pos = pos)
  class(cspt) <- c("csptable", class(cspt))
  cspt
}


#' Print table
#'
#' A print method for \code{sptable} and \code{slice} objects
#'
#' @param x A \code{sptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.sptable <- function(x, ...) {
  vars      <- attr(x, "vars")
  vars_cond <- attr(x, "vars_cond")
  nchr  <- sum(.map_int(vars, function(s) nchar(s))) + length(vars) + nchar("Vars:") - 1
  N     <- length(x)
  cells <- names(x)
  ## for (v in seq_along(vars_cond)) {
  ##   cat("",vars_cond[v], "\n")
  ## }
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "slice")) {
    cat("", paste(paste(names(vars_cond), "= "), vars_cond, sep = "", collapse = ", "), "\n")    
  }
  ## cat("", paste0(vars_cond, collapse = ", "), "\n")
  # cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  print(utils::head(x, n = N))
  ## for (i in 1:N) {
  ##   cat(paste0(cells[i], " : ", x[i]),"\n")
  ## }
}

#' Print method for conditional sparse tables
#'
#' @param x A \code{csptable} object
#' @param ... Not used (for S3 compatability)
#' @export
print.csptable <- function(x, ...) {
  for (i in seq_along(x)) {print(x[[i]]); cat("\n")}
}
