## ---------------------------------------------------------
##                    TESTING!!!
## ---------------------------------------------------------
## Test the merge function to the extreme. It is afterall
## the backbone!!!

## library(dplyr)
## library(igraph)
## d <- tgp_dat[1:500, 5:70]
## d <- d %>% select(5:14)
## colnames(d) <- letters[1:10]
## dm <- d %>% as.matrix()

# Vars: b-a
# Vars: c-a
# - FIND THE CORRECT VALUE!! (Maybe using Sørens API ?)
## pb_a1 <- parray(sptable(dm[, c("b", "a")]), "a")
## pb_a2 <- parray(sptable(dm[, c("a", "b")]), "a")
## pc_a1 <- parray(sptable(dm[, c("c", "a")]), "a")
## pc_a2 <- parray(sptable(dm[, c("a", "c")]), "a")
## merge(parray(sptable(dm[, c("a"), drop = FALSE])), pc_a)

## merge(pb_a1, pc_a1)
## merge(pb_a1, pc_a2)

# Vars: a-b-c-d
# Vars: b-d-a-e
## pabc <- parray(sptable(dm[, c("a", "b", "c")]))
## pcad <- parray(sptable(dm[, c("c", "a", "d")]))
## merge(pabc, pcad)


## ---------------------------------------------------------
##                   EXAMPLE 1
##                 (igraph DAG)
## ---------------------------------------------------------

#### FIX --- SOME CLIQUE POTENTIALS MAY BE SET TO THE IDENTITY
#### SINCE NO CONDITIOANLS CORRESPOND TO THIS DUE TO TRIANGULATION
#### TAKE THIS INTO ACCOUNT!!!

## library(dplyr)
## library(igraph)
## d <- tgp_dat[1:500, 5:70]
## set.seed(7)
## g <- make_tree(7)
## g <- g + edge(4, 7)
## g <- g + edge(5, 6)
## g <- add_vertices(g, 3)
## g <- g + edge(9, 10)
## g <- set.vertex.attribute(g, "name", value = letters[1:10])
## plot(g)

## d <- d %>% select(5:14)
## colnames(d) <- letters[1:10]
## d

## jt <- new_jt(g, d)
## plot_jt(jt)

## m  <- send_messages(jt)
## plot_jt(m)

## m2 <- send_messages(m)
## plot_jt(m2)

## # Sanity check
## sum(m2$charge$C$C1)
## sum(m2$charge$C$C2)

## m3 <- send_messages(m2)
## plot_jt(m3)

## m4 <- send_messages(m3)
## m4

## m4$charge

## sum(m4$charge$C$C3)
## sum(m4$charge$S$S3)

## attr(m4$charge$C[[4]], "vars")

## pa <- parray(sptable(d %>%  select(f, c, e) %>%  as.matrix()))
## pa
## m4$charge$C[[4]]


## ---------------------------------------------------------
##                   EXAMPLE 2
##               (Decomposable MRF)
## ---------------------------------------------------------
## library(dplyr)
## d <- digits[, 5:50] %>% as_tibble()
## g <- fit_graph(d, trace = FALSE, q = 0)
## for (k in 1:50) g <- walk(g, d)

## gjt  <- new_jt(g, d, flow = sum)
## par(mfrow = 1:2)
## plot(g, vertex.size = 2)
## plot_jt(gjt, vertex.size = 15)

## m <- send_messages(gjt)
## while (attr(m, "direction") != "FULL") m <- send_messages(m)

## c1 <- m$charge$C$C1; c1
## v1 <- attr(c1, "vars")
## p1 <- parray(sptable(as.matrix(d[, v1])))[names(c1)]
## round(c1 - p1, 10)

## c2 <- m$charge$C$C2; c2
## v2 <- attr(c2, "vars")
## p2 <- parray(sptable(as.matrix(d[, v2])))[names(c2)]
## round(c2 - p2, 10)

## c3 <- m$charge$C$C3; c3
## v3 <- attr(c3, "vars")
## p3 <- parray(sptable(as.matrix(d[, v3])))[names(c3)]
## round(c3 - p3, 10)

## c4 <- m$charge$C$C4; c4
## v4 <- attr(c4, "vars")
## p4 <- parray(sptable(as.matrix(d[, v4])))[names(c4)]
## round(c4 - p4, 10)

## c5 <- m$charge$C$C5; c5
## v5 <- attr(c5, "vars")
## p5 <- parray(sptable(as.matrix(d[, v5])))[names(c5)]
## round(c5 - p5, 10)

## c40 <- m$charge$C$C40; c40
## v40 <- attr(c40, "vars")
## p40 <- parray(sptable(as.matrix(d[, v40])))[names(c40)]
## round(c40 - p40, 10)

## ---------------------------------------------------------
##                   EXAMPLE 3
##               (Decomposable MRF)
## ---------------------------------------------------------

## d <- tgp_dat[1:500, 5:70] # with 200 additional edges.  Rounding errors.
## d <- tgp_dat[1:500, 5:50] # Rounding errors - but close! (38 cliques)
## d <- tgp_dat[1:500, 5:30] # Rounding errors - way off! (20 cliques)

## library(dplyr)
## d <- tgp_dat[1:500, 5:10]
## colnames(d) <- c(letters, LETTERS, 1:9)[1:ncol(d)]

## g  <- fit_graph(d, trace = FALSE)
## e  <- c(d = "A", b = "C")
## jt <- new_jt(g, d, e, flow = sum)

## plot(jt)
## print(jt)

## par(mfrow = c(1, 2))
## plot.gengraph(g, structure(rep("orange", ncol(d)), names = colnames(d)), vertex.size = 10)
## plot_jt(jt, vertex.size = 15)


## m <- send_messages(jt)
## while (attr(m, "direction") != "FULL") m <- send_messages(m)

## c1 <- m$charge$C$C1; c1
## v1 <- attr(c1, "vars")
## parray(sptable(as.matrix(d[, v1])))[names(c1)]

## c2 <- m$charge$C$C2; c2
## v2 <- attr(c2, "vars")
## parray(sptable(as.matrix(d[, v2])))[names(c2)]

## c3 <- m$charge$C$C3; c3
## v3 <- attr(c3, "vars")
## parray(sptable(as.matrix(d[, v3])))[names(c3)]

## c4 <- m$charge$C$C4; c4
## v4 <- attr(c4, "vars")
## parray(sptable(as.matrix(d[, v4])))[names(c4)]

## c5 <- m$charge$C$C5; c5
## v5 <- attr(c5, "vars")
## parray(sptable(as.matrix(d[, v5])))[names(c5)]
