## ---------------------------------------------------------
##                 EXPORTED FUNCTIONS
## ---------------------------------------------------------

#' Sparse table
#'
#' Returns a sparse contingency table for the variables in \code{x} as a vector .
#'
#' @param x Character matrix
#' @seealso \code{\link{parray}}
#' @examples
#' sptable(as.matrix(digits[, 1:2]))
#' @export
sptable <- function(x) {
  stopifnot(is.matrix(x))
  sptab <- n_a(x)
  class(sptab) <- c("sptable", class(sptab))
  return(sptab)
}

#' Conditional probability table
#'
#' Returns a sparse conditional probability table conditioned on variables in \code{b}.
#'
#' @param x A sparse table obtained from \code{sptable}
#' @param y Character vector with variables to condition on
#' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{sptable} with no conditioning variables, i.e. the marginal table.
#' @return A \code{sptable} object
#' @seealso \code{\link{sptable}}
#' @examples
#' sp  <- sptable(as.matrix(digits[, 1:3]))
#' psp <- parray(sp, c("V1", "V2"))
#' sum(psp[1:length(letters[1:17])]) # V3 has 17 levels.
#' @export
parray <- function(x, y = NULL) {
  stopifnot(inherits(x, "sptable"))
  if (is.null(y)) {
    xs <- x / sum(x)
    class(xs) <- class(x)
    return(xs)
  }
  pos  <- match(y, attr(x, "vars"))
  conf <- find_cond_configs(x, pos)
  conditional_list <- split(names(conf), conf)
  parr <- unlist(unname(lapply(conditional_list, function(e) {
    x[e] / sum(x[e])
  })))
  attr(parr, "vars") <- attr(x, "vars")
  class(parr) <- class(x)
  return(parr)
}

#' Print Sparse Table
#'
#' A print method for \code{sptable} and \code{slice_sptable} objects
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
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  if (inherits(x, "slice")) {
    cat("", paste(paste(names(vars_cond), "= "), vars_cond, sep = "", collapse = ", "), "\n")    
  }
  cat(" Vars:", paste0(vars, collapse = "-"), "\n")
  cat("", paste0(rep("-", nchr), collapse = ""), "\n")
  for (k in seq_along(cells)) {
    cat(cells[k], ":", x[k], "\n")
  }
  invisible(x)
}


## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
.set_as_sptable <- function(x) {
  structure(x, class = c("sptable", class(x)))
}

#' @export
`[<-.sptable` <- function(x, i, value) {
  NextMethod()
}

#' @export
`[.sptable` <- function(x, i) {
  structure(.set_as_sptable(NextMethod()) , vars = attr(x, "vars"))
}

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

reposition_names <- function(x, pos) {
  # x : named list
  structure(x, names =.map_chr(names(x), function(y) {
    paste(.split_chars(y)[pos], collapse = "")
  }))
}


merge.sptable <- function(p1, p2, op = "*") {
  # p1, p2 : sptable objects
  
  stopifnot(op %in% c("*", "/"))
  
  v1     <- attr(p1, "vars")
  v2     <- attr(p2, "vars")
  sep    <- intersect(v1, v2)
  
  # If no variables in common it is easy
  if (!neq_empt_chr(sep)) {
    spt <- lapply(seq_along(p1), function(i) {
      p1i <- p1[i]
      structure(do.call(op, list(p2, p1i)),
        names = paste(names(p2), names(p1[i]), sep = "")
      )
    })
    spt <- unlist(spt)
    attr(spt, "vars") <- c(v2, v1)
    class(spt) <- c("sptable", class(spt))
    return(spt)
  }

  pos1   <- match(sep, v1)
  pos2   <- match(sep, v2)

  cf1    <- find_cond_configs(p1, pos1)
  cf2    <- find_cond_configs(p2, pos2)

  scf1   <- split(names(cf1), cf1)
  scf2   <- split(names(cf2), cf2)

  ## TODO: Wrap this in a function
  ## ---------------------------------------------------------
  
  # No need for repositioning if leng(sep) > 1 (they must agree then).
  if (length(sep) > 1L) {
    pos1_sep <- structure(seq_along(pos1), names = v1[pos1])
    spos2    <- sort(pos2)
    pos2_sep <- structure(seq_along(spos2), names = v2[sort(spos2)])
    pos2_new <- pos2_sep[names(pos1_sep)]
    scf2     <- reposition_names(scf2, pos2_new)
  }
  ## ---------------------------------------------------------
  
  # Those not in sc_sep are structural zeroes!
  sc_sep  <- intersect(names(scf1), names(scf2))
  scf1    <- scf1[sc_sep]
  scf2    <- scf2[sc_sep]
   
  spt <- lapply(sc_sep, function(x) {

    scf1_x <- scf1[[x]]
    scf2_x <- scf2[[x]]

    p1x    <- p1[scf1_x]
    p2x    <- p2[scf2_x]

    res    <- vector("double", length = length(p1x) * length(p2x))
    res_names <- vector("character", length = length(p1x) * length(p2x))

    iter <- 1L
    for (i in seq_along(p1x)) {

      for (j in seq_along(p2x)) {
        p1i <- p1x[i]
        p2j <- p2x[j]

        p2j_name  <- names(p2j)
        new_name  <- paste0(names(p1i), str_rem(p2j_name, pos2), collapse = "")

        res[iter] <- if (isTRUE(all.equal(p2j, 0))) 0 else do.call(op, list(p1i, p2j))
        res_names[iter] <- new_name
        iter <- iter + 1L
      }
    }
    structure(res, names = res_names)
  })

  
  spt <- unlist(spt)
  
  attr(spt, "vars") <- c(v1, setdiff(v2, v1))
  class(spt) <- c("sptable", class(spt))
  
  return(spt)
}

marginalize <- function(p, s, flow = sum) UseMethod("marginalize")

marginalize.sptable <- function(p, s, flow = sum) {
  
  v <- attr(p, "vars")
  if (any(is.na(match(s, v)))) stop("Some variables in s are not in p")

  marg_vars <- setdiff(v, s)
  pos <- match(marg_vars, v)

  cf  <- find_cond_configs(p, pos)
  scf <- split(names(cf), cf)

  spt <- lapply(scf, function(e) {
    flow(p[e])
  })
  
  spt <- unlist(spt)
  attr(spt, "vars") <- marg_vars
  class(spt) <- c("sptable", class(spt))
  return(spt)
}



## library(dplyr)
## d <- as.matrix(tgp_dat[, 7:50])
## colnames(d) <- letters[1:44]

## sp <- sptable(d[, 1:30])
## p  <- parray(sp)
## m  <- marginalize(p, letters[1:10], sum)
## sum(m)

## p1 <- sptable(d[, 1, drop = FALSE])
## p <- sptable(d[, 1:2, drop = FALSE])
## marginalize(p, c("b"), max)
## mp <- merge(parray(p1), parray(p2), "*")
## merge(parray(p2), mp)

## p <- parray(sptable(d[, 1:5]))
## marginalize(p, c("c", "e"), max)
## slice_sptable(p, c("A" = 5))

## p1 <- sptable(d[, c(2,1,3), drop = FALSE])
## p2 <- sptable(d[, 1:5, drop = FALSE])
## merge(p1, p2)



## #' Slice of a sparse table
## #'
## #' Returns the b-slice of a conditional sparse contingency table for the variables in \code{x} as a vector.
## #'
## #' @param x A \code{sptable} object
## #' @param b A named vector of indicies for which the variables are fixed
## #' @description The names of \code{b} are the fixed values of the variables corresponding to the indicies
## #' @seealso \code{\link{sptable}}
## #' @examples
## #' sp <- sptable(as.matrix(digits[, 1:3]))
## #' # print(sp)
## #' slice_sptable(sp, c(e = 3))
## #' @export
## slice_sptable <- function(x, b) {
##   stopifnot(inherits(x, "sptable"))
##   sl <- n_b(x, b)
##   class(sl) <- c("slice", class(x))
##   return(sl)
## }



## #' Conditional sparse table
## #'
## #' Returns a conditional sparse contingency table conditioned on variables in \code{b}.
## #'
## #' @param x A sparse table obtained from \code{sptable}
## #' @param y Character vector with variables to condition on
## #' @details If \code{y} is \code{NULL}, \code{x} is just converted to a \code{csptable} with no conditioning variables, i.e. the marginal table.
## #' @seealso \code{\link{sptable}}, \code{\link{slice_sptable}}
## #' @examples
## #' sp <- sptable(as.matrix(digits[, 1:3]))
## #' sp
## #' length(sp)
## #' csp <- csptable(sp, c("V3"))
## #' csp
## #' length(csp)
## #' @export
## csptable <- function(x, y) {
##   # x : sptable
##   # y : conditional variables
##   if (is.null(y)) {
##     cspt <- structure(list(x), names = "marg")
##     class(cspt) <- c("csptable", class(cspt))
##     return(cspt)
##   }
  
##   # check if y is in the names of x
##   if (!all(y %in% attr(x, "vars"))) stop("Some names in 'y' are not in 'x'")
  
##   pos     <- match(y, attr(x, "vars"))
##   configs <- find_cond_configs(x, pos)
##   cspt <- lapply(configs, function(z) {
##     b  <- structure(pos, names = .split_chars(z))
##     sl <- slice_sptable(x, b)
##     return(sl)
##   })
##   cspt <- structure(cspt, names = configs, pos = pos)
##   class(cspt) <- c("csptable", class(cspt))
##   cspt
## }


## #' Print method for conditional sparse tables
## #'
## #' @param x A \code{csptable} object
## #' @param ... Not used (for S3 compatability)
## #' @export
## print.csptable <- function(x, ...) {
##   for (i in seq_along(x)) {print(x[[i]]); cat("\n")}
## }
