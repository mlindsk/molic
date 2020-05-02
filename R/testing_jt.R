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


## p <- parray(sptable(dm[, c("b", "a")]), "a")

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

## e  <- c(a = "T", b = "C", c = "A", j = "A", g = "T")
## jt <- new_jt(g, d, e, flow = "max")
## m  <- send_messages(jt, "max")
## while (attr(m, "direction") != "FULL") m <- send_messages(m, "max")
## plot(m)
## attr(m, "mpe")
## m$charge$C
## m <- send_messages(jt, "max")
## m <- send_messages(m, "max")
## plot(m2)

## # Sanity check
## sum(m2$charge$C$C1)
## m2$charge$C$C1
## molic::parray(sptable(d %>% select(a, b, c, d) %>% as.matrix()))
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

## gjt  <- new_jt(g, d, flow = "max")
## par(mfrow = 1:2)
## plot(g, vertex.size = 2)
## plot_jt(gjt, vertex.size = 15)

## m <- send_messages(gjt, "max")
## while (attr(m, "direction") != "FULL") m <- send_messages(m, "max")
## attr(m, "mpe")

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
## d <- tgp_dat[1:500, 3:5]
## colnames(d) <- c(letters, LETTERS, 1:9)[1:ncol(d)]

## g  <- fit_graph(d, trace = FALSE)
## # e  <- c(d = "A", b = "C")
## jt <- new_jt(g, d, flow = "max")
## m <- send_messages(jt, "max")
## while (attr(m, "direction") != "FULL") m <- send_messages(m, "max")
## attr(m, "mpe")

## plot(jt)
## print(jt)

## par(mfrow = c(1, 2))
## plot.gengraph(g, structure(rep("orange", ncol(d)), names = colnames(d)), vertex.size = 10)
## plot(jt, vertex.size = 15)


## m <- send_messages(jt)

## plot(m)
## while (attr(m, "direction") != "FULL") m <- send_messages(m)

## c1 <- m$charge$C$C1; c1
## v1 <- attr(c1, "vars")
## parray(sptable(as.matrix(d[, v1])))

## c2 <- m$charge$C$C2; c2
## v2 <- attr(c2, "vars")
## parray(sptable(as.matrix(d[, v2])))

## c3 <- m$charge$C$C3; c3
## v3 <- attr(c3, "vars")
## parray(sptable(as.matrix(d[, v3])))

## c4 <- m$charge$C$C4; c4
## v4 <- attr(c4, "vars")
## parray(sptable(as.matrix(d[, v4])))[names(c4)]

## c5 <- m$charge$C$C5; c5
## v5 <- attr(c5, "vars")
## parray(sptable(as.matrix(d[, v5])))[names(c5)]


## ---------------------------------------------------------
##                    EXAMPLE 4
## ---------------------------------------------------------
## library(dplyr)
## library(gRbase)
## library(gRain)

## ---------------------------------------------------------
##                      DATA
## ---------------------------------------------------------

## g <- dag(
##   "asia",
##   "smoke",
##   c("tub", "asia"),
##   c("either", "tub", "lung"),
##   c("lung", "smoke"),
##   c("bronc", "smoke"),
##   c("xray", "either"),
##   c("dysp", "either", "bronc")
## )

## plot(g)

## data(chestSim500, package = "gRbase")
## d_gr <- chestSim500 # %>% molic:::to_single_chars() %>% as.data.frame()
## d_mc <- d_gr %>% mutate_all(as.character)

## ---------------------------------------------------------
##                     GRAIN
## ---------------------------------------------------------
## cpt   <- extractCPT(d_gr, g)
## jt_gr <- compileCPT(cpt)
## jt_gr <- grain(jt_gr)
## jt_gr <- gRbase::compile(jt_gr, propagate = TRUE, smooth = 0)
## querygrain(jt_gr, nodes = c("xray", "either"), type = "joint")

## Without evidence
##  ---------------- 
##  Vars: xray-either 
##  ---------------- 
## aa : 0.8967073 
## ba : 0.04393666 
## bb : 0.059356 


## ## ---------------------------------------------------------
## ##                      MOLIC
## ## ---------------------------------------------------------
## jt_mc <- new_jt(as(g, "igraph"), d_mc)
## plot(jt_mc)
## while(attr(jt_mc, "direction") != "FULL") jt_mc <- send_messages(jt_mc)


## ## C1:
## # a:
## jt_mc$charge$C$C1
## querygrain(jt_gr, nodes = c("asia", "tub"), type = "joint")


## ## # Sanity check:
## at    <- as.matrix(d_mc[, c("asia", "tub")])
## sp_at <- sptable(at)
## molic::parray(sp_at)

## # C2
## jt_mc$charge$C$C2
## querygrain(jt_gr, nodes = c("either", "tub"), type = "joint")

## # C3
## jt_mc$charge$C$C3
## querygrain(jt_gr, nodes = c("smoke", "lung", "either"), type = "joint")

## # C4
## jt_mc$charge$C$C4
## querygrain(jt_gr, nodes = c("smoke", "bronc", "either"), type = "joint")


## # C5:
## jt_mc$charge$C$C5
## querygrain(jt_gr, nodes = c("dysp", "either", "bronc"), type = "joint")

## ---------------------------------------------------------
## ---------------------------------------------------------
## ---------------------------------------------------------
## ---------------------------------------------------------
## ---------------------------------------------------------
## ---------------------------------------------------------

## library(igraph)
## library(dplyr)
## g <- make_tree(8) %>% set_vertex_attr("name", value = LETTERS[1:8])

## library(gRbase)

## g <- dag(
##   "asia",
##   "smoke",
##   c("tub", "asia"),
##   c("either", "tub", "lung"),
##   c("lung", "smoke"),
##   c("bronc", "smoke"),
##   c("xray", "either"),
##   c("dysp", "either", "bronc")
## )

## g <- as(g, "igraph")
## saveRDS(g, "g.Rds")

## g <- readRDS("g.Rds")
## plot(g)

## ---------------------------------------------------------
##                    MOLIC ASIA 
## ---------------------------------------------------------
## library(igraph)
## library(dplyr)
## d_gr <- readRDS("d.RDS") %>% molic:::to_single_chars() %>% as.data.frame()
## d_mc <- d_gr %>% mutate_all(as.character) %>% as_tibble()

## g <- readRDS("g.Rds")
## is_dag(g)
## plot(g)

## ## pg  <- parents_igraph(g)
## ## mg  <- moralize_igraph(g, pg)
## ## mgt <- as.undirected(triangulate_igraph(mg))

## ## par(mfrow = c(1, 3))
## ## plot(g)
## ## plot(mg)
## ## plot(mgt)

## jt_mc <- new_jt(g, d_mc)
## jt_mc$charge$C
## jt_mc$cliques

## plot(jt_mc)
## while(attr(jt_mc, "direction") != "collect") jt_mc <- send_messages(jt_mc)

## while(attr(jt_mc, "direction") != "FULL") jt_mc <- send_messages(jt_mc)

## jt_mc$charge$C[1:3]

## msg <- marginalize(jt_mc$charge$C[[1]], "asia")
## merge(jt_mc$charge$C[[2]], msg)


## jt_mc <- send_messages(jt_mc)
## plot(jt_mc)
## jt_mc$charge$C[1:3]

## jt_mc <- send_messages(jt_mc)
## plot(jt_mc)
## jt_mc$charge$C[1:3]

## jt_mc <- send_messages(jt_mc)
## plot(jt_mc)
## jt_mc$charge$C[1:3]

## jt_mc <- send_messages(jt_mc)
## plot(jt_mc)
## jt_mc$charge$C[1:3]


## jt_mc$charge$C$C2
## parray(sptable(as.matrix(d_mc[, c("either", "tub")])))
