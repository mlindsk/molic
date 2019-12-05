## ---------------------------------------------------------
##                NON-EXPORTED HELPERS
## ---------------------------------------------------------
neq_null     <- function(x) !is.null(x)
neq_empt_chr <- function(x) !identical(x, character(0))
neq_empt_num <- function(x) !identical(x, numeric(0))
neq_empt_int <- function(x) !identical(x, integer(0))
neq_empt_lst <- function(x) !identical(x, list())
es_to_vs     <- function(e) strsplit(e, "\\|")
vs_to_es     <- function(e) lapply(e, paste0, collapse = "|")
rev_es       <- function(e) sapply(es_to_vs(e), function(x) paste0(rev(x), collapse = "|"))
sort_        <- function(x) paste0(sort(x), collapse = "|")
.split_chars <- function(x) unlist(strsplit(x, ""))
'%ni%'       <- Negate('%in%')

## ---------------------------------------------------------
##                EXPORTED HELPERS
## ---------------------------------------------------------

#' A test for decomposability in undirected graphs
#'
#' This function returns \code{TRUE} if the graph is decomposable and \code{FALSE} otherwise
#'
#' @param adj Adjacency list of an undirected graph
#' @examples
#' # 4-cycle:
#' adj1 <- list(a = c("b", "d"), b = c("a", "c"), c = c("b", "d"), d = c("a", "c"))
#' is_decomposable(adj1) # FALSE
#' # Two triangles:
#' adj2 <- list(a = c("b", "d"), b = c("a", "c", "d"), c = c("b", "d"), d = c("a", "c", "b"))
#' is_decomposable(adj2) # TRUE
#' @export
is_decomposable <- function(adj) {
  m <- try(mcs(adj), silent = TRUE)
  if( inherits(m, "list") ) return(TRUE)
    else return(FALSE)
}


#' Make a complete graph
#'
#' A helper function to make an adjacency list corresponding to a complete graph
#'
#' @param nodes A character vector containing the nodes to be used in the graph
#' @examples
#' d  <- digits[, 5:8]
#' cg <- make_complete_graph(colnames(d))
#' @export
make_complete_graph <- function(nodes) {
  structure(lapply(seq_along(nodes), function(k) {
    nodes[-which(nodes == nodes[k])]
  }), names = nodes)
}

#' Make a null graph
#'
#' A helper function to make an adjacency list corresponding to a null graph (no edges)
#'
#' @param nodes A character vector containing the nodes to be used in the graph
#' @examples
#' d  <- digits[, 5:8]
#' ng <- make_null_graph(colnames(d))
#' @export
make_null_graph <- function(nodes) {
  structure(lapply(seq_along(nodes), function(x) {
    character(0)
  }), names = nodes)
}
