## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------

## MAPS
.map_chr     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
.map_int     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
.map_dbl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
neq_null     <- function(x) !is.null(x)

## STRINGS
es_to_vs     <- function(e) strsplit(e, "\\|")
vs_to_es     <- function(e) lapply(e, paste0, collapse = "|")
rev_es       <- function(e) .map_chr(es_to_vs(e), function(x) paste0(rev(x), collapse = "|"))
sort_        <- function(x) paste0(sort(x), collapse = "|")
.split_chars <- function(x) unlist(strsplit(x, ""))

## SETS
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
neq_null     <- function(x) !is.null(x)
'%ni%'       <- Negate('%in%')
push         <- function(l, el, name = NULL) c(l, structure(list(el), names = name))

# `%[int%`   <- function(a, x) if (neq_empt_int(x)) return(a[x]) else return(a)
# 


only_single_chars <- function(A) {
  for (i in seq_along(nrow(A))) {
    for (j in seq_along(ncol(A)))
      if ( nchar(A[i,j]) != 1L ) return(FALSE)
  }
  return(TRUE)
}


## ---------------------------------------------------------
##                     EXPORTED HELPERS
## ---------------------------------------------------------

#' To Single Chars
#'
#' Convert all values in a data frame or matrix of characters to a single character representation
#'
#' @param x Data frame or matrix of characters
#' @examples
#' d <- data.frame(x = c("11", "2"), y = c("2", "11"))
#' to_single_chars(d)
#' @export
to_single_chars <- function(x) {
  ## Implicitly assumes that no columns has more than length(letters) = 26 unique levels
  apply(x, 2, function(z) {
    f <- as.factor(z)
    levels(f) <- letters[1:length(levels(f))]
    as.character(f)
  })
}
