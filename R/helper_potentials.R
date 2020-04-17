pot_merge <- function(p1, p2, op = "*") {
  stopifnot(op %in% c("*", "/"))
  v1   <- attr(p1, "vars")
  v2   <- attr(p2, "vars")
  sep  <- intersect(v1, v2)
  pos1 <- match(sep, attr(p1, "vars"))
  pos2 <- match(sep, attr(p2, "vars"))
  conf1 <- find_cond_configs(p1, pos1)
  conf2 <- find_cond_configs(p2, pos2)
  sc1    <- split(names(conf1), conf1)
  sc2    <- split(names(conf2), conf2)
  sc_sep <- intersect(names(sc1), names(sc2))
  if (length(sc_sep) == 1L && sc_sep == "") stop("Do we need these cases? If yes, implement it.")
  # Note: Those not in sc_sep are structural zeroes
  sc1    <- sc1[sc_sep]
  sc2    <- sc2[sc_sep]
  pot <- lapply(sc_sep, function(x) {
    sc1_x <- sc1[[x]]
    sc2_x <- sc2[[x]]
    p1x   <- p1[sc1_x]
    p2x   <- p2[sc2_x]
    res       <- vector("double", length = length(p1x) * length(p2x))
    res_names <- vector("character", length = length(p1x) * length(p2x))
    iter <- 1L
    for (i in seq_along(p1x)) {
      for (j in seq_along(p2x)) {
        p1i <- p1x[i]
        p2j <- p2x[j]
        p2j_name <- names(p2j)
        new_name <- paste0(names(p1i), str_rem(p2j_name, pos2), collapse = "")
        res[iter] <- if (op == "*") log(p1i) + log(p2j) else log(p1i) - log(p2j)
        res_names[iter] <- new_name
        iter <- iter + 1L
      }
    }
    structure(res, names = res_names)
  })
  pot <- unlist(pot)
  attr(pot, "vars") <- c(v1, setdiff(v2, v1))
  class(pot) <- c("sptable", class(pot))
  return(pot)
}

## d <- as.matrix(tgp_dat[, 7:20])
## colnames(d) <- letters[1:14]

## pa_b <- parray(sptable(d[, 1:5]), c("b"))
## pb_c <- parray(sptable(d[, c(2,4,6)]), NULL)
## pa_b
## pb_c
## pot_merge(pa_b, pb_c)
## length(pot_merge(pa_b, pb_c))
