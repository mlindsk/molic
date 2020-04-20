merge.sptable <- function(p1, p2, op = "*") {
  # p1, p2 : sptable objects
  stopifnot(op %in% c("*", "/"))
  v1     <- attr(p1, "vars")
  v2     <- attr(p2, "vars")
  sep    <- intersect(v1, v2)
  pos1   <- match(sep, v1)
  pos2   <- match(sep, v2)
  cf1    <- find_cond_configs(p1, pos1)
  cf2    <- find_cond_configs(p2, pos2)
  scf1    <- split(names(cf1), cf1)
  scf2    <- split(names(cf2), cf2)
  sc_sep <- intersect(names(scf1), names(scf2))
  if (length(sc_sep) == 1L && sc_sep == "") stop("Do we need these cases? If yes, implement it.")
  # Note: Those not in sc_sep are structural zeroes
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
        res[iter] <- do.call(op, list(p1i, p2j))
        # if (op == "*") log(p1i) + log(p2j) else log(p1i) - log(p2j)
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

marginalize <- function(p, y, flow = sum, ...) UseMethod("marginalize")

marginalize.sptable <- function(p, y, flow = sum) {
  v <- attr(p, "vars")
  if (any(is.na(match(y, v)))) stop("some variables in y are not in p")
  marg_vars <- setdiff(v, y)
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
