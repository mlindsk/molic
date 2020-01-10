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
rev_es       <- function(e) vapply(es_to_vs(e), function(x) paste0(rev(x), collapse = "|"), character(1L))
sort_        <- function(x) paste0(sort(x), collapse = "|")
.split_chars <- function(x) unlist(strsplit(x, ""))
'%ni%'       <- Negate('%in%')

## ---------------------------------------------------------
##                EXPORTED HELPERS
## ---------------------------------------------------------
#' Probability mass function for a decomposable graphical model
#'
#' @param A A character matrix of data
#' @param adj Adjacency list or gengraph object of a decomposable graph. See \code{fit_graph}.
#' @param logp Logical; if TRUE, probabilities p are given as log(p).
#' @return A function - the probability mass function corresponding
#' to the decomposable graph \code{adj} using \code{A} to estimate
#' the probabilities.
#' @details It should be noted, that \code{pmf} is a closure function; i.e. its return
#' value is a function. Once \code{pmf} is called, one can query probabilities from the
#' returned function. If the probability of an observation is zero, the function will return
#' \code{NA} if \code{logp} is set to \code{TRUE} and zero otherwise.
#' @examples
#' \dontrun{
#' library(dplyr)
#' 
#' # All handwritten digits that have true class equal to a "1".
#' d <- digits %>% # subset(digits, class == "1")
#'   filter(class == "1") %>%
#'   select(-class)
#'
#' # A handwritten digit with true class equal to "1"
#' z1 <- digits %>%
#'   filter(class == "1") %>%
#'   select(-class) %>%
#'   slice(5) %>%
#'   unlist()
#' 
#' # A handwritten digit with true class equal to "7"
#' z7 <- digits %>%
#'   filter(class == "7") %>%
#'   select(-class) %>%
#'   slice(1) %>%
#'   unlist()
#' 
#' # Fit an interaction graph
#' g <- fit_graph(d, trace = FALSE)
#' plot(g)
#'
#' g <- g %>%
#'   adj_lst()
#'
#' # Probability in class "1"
#' p1 <- pmf(d %>% as.matrix(), g)
#' p1(z7)
#' p1(z1)
#'
#' # Probability on component 23 in class "1"
#' cmp   <- components(g)
#' cmp23 <- cmp[[23]]
#' print(cmp23)
#' p1_23 <- pmf(d %>% select(cmp23) %>% as.matrix(), g[cmp23])
#' p1_23(z7)
#' p1_23(z1)
#'
#' }
#' @export
pmf <- function(A, adj, logp = FALSE) {
  if (!is_decomposable(adj)) stop("The graph corresponding to adj is not decomposable!")
  if (!setequal(colnames(A), names(adj))) stop("Variables in A and the names of adj do not conform!")
  RIP   <- rip(adj)
  cms   <- RIP$C
  sms   <- RIP$S
  ncms <- a_marginals(A, cms)
  nsms <- a_marginals(A, sms)
  .pmf <- function(y) {
    ny <- vapply(seq_along(ncms), FUN.VALUE = 1, FUN =  function(i) {
      nci    <- ncms[[i]]
      nsi    <- nsms[[i]]
      yci    <- y[match(attr(nci, "vars"), names(y))]
      ycinam <- paste0(yci, collapse = "")
      nciy   <- nci[ycinam] # NA if y is not seen on ci
      if (i == 1L) return(log(nciy))
      nsiy <- nsi[1]
      if (length(nsi) > 1) {
        ysi    <- y[match(attr(nsi, "vars"), names(y))]
        nsinam <- paste0(ysi, collapse = "")
        nsiy   <- nsi[nsinam] # NA if not seen on si
      } 
      return(log(nciy) - log(nsiy))
    })
    if (anyNA(ny)) return(ifelse(logp, NA, 0L)) # The observation was not seen on some marginals
    logprob <- sum(ny) - log(nrow(A))
    return(ifelse(logp, logprob , exp(logprob)))
  }
}


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
