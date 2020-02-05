## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
.map_chr     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = character(1), ...)
.map_int     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = integer(1), ...)
.map_dbl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = numeric(1), ...)
.map_lgl     <- function(x, fun, ...) vapply(X = x, FUN = fun, FUN.VALUE = logical(1), ...)
neq_null     <- function(x) !is.null(x)
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
es_to_vs     <- function(e) strsplit(e, "\\|")
vs_to_es     <- function(e) lapply(e, paste0, collapse = "|")
rev_es       <- function(e) .map_chr(es_to_vs(e), function(x) paste0(rev(x), collapse = "|"))
sort_        <- function(x) paste0(sort(x), collapse = "|")
.split_chars <- function(x) unlist(strsplit(x, ""))
'%ni%'       <- Negate('%in%')
#push         <- function(l, el, name = NULL) c(l, structure(list(el), names = name))

