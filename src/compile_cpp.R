library(microbenchmark)
library(igraph)
Rcpp::sourceCpp("tmp_misc.cpp")

## ------------------------------
## MCS EX1
## ------------------------------
A <- c("B", "C")
B <- c("A", "C")
C <- c("A", "B", "D")
D <- c("C")
E <- c("F")
F <- c("E")
N <- 6
adj <- list(A, B, C, D, E, F)
names(adj) <- LETTERS[1:N]
nods <- c("A", "B","A", "C","B", "C","C", "D","E", "F")
G    <- make_graph(nods, directed = FALSE)
A    <- igraph::as_adjacency_matrix(G)
plot(G)
microbenchmark(mcs_(adj), mcs(A), gRbase::mcs(G), times = 2000)
 
## ------------------------------
## MCS EX2
## ------------------------------
source(file = "/home/mads/Documents/phd/publications/molic/R/rip.R")
N <- 30
q <- vector(length = 0L)
for(i in 2:N) q <- c(q, rep(i, 2))
nods <- as.character(q[1:(length(q)-3)])
nods <- c("1", nods)
nods <- paste0("x", nods)
adj  <- vector("list", length = 0L)
for( k in 1:(N-2) ) {
  adj[[k]] <- paste0("x", k+1)
}
names(adj) <- unique(nods)[-(N-1)]
G   <- make_graph(nods, directed = FALSE)
A   <- igraph::as_adjacency_matrix(G)
microbenchmark(mcs_(adj), mcs(A), gRbase::mcs(G), times = 5)

## -------------------------------------
## PERFECT SEQUENCE
## -------------------------------------
microbenchmark(perfect_sequence(A, mcs(A)), perfect_seq(adj, mcs_(adj)), times = 50)
identical(perfect_seq(adj, mcs_(adj)), perfect_sequence(A, mcs(A)))

## -------------------------------------
## PERFECT SEQUENCE OF CLIQUES
## -------------------------------------
is_subseteq(c("1", "2", "3", "4"), c("1", "2", "3", "4"))

## -------------------------------------
## SET OPERATIONS
## -------------------------------------
v <- nods
w <- v[-sample(1:length(v), length(v) - 5)]

microbenchmark(setdiff(v, w), set_diff(v, w))
microbenchmark(union(v, w), set_union(v, w))
microbenchmark(setequal(v, w), set_eq(v, w))
microbenchmark(intersect(v, w), set_intersect(v, w))

## -------------------------------------
## TABLE
## -------------------------------------
N <- 1000
X <- as.character(sample(letters[1:20], N, replace = TRUE))
microbenchmark(table(X), table_(X))

## -------------------------------------
## PASTE ROWS
## -------------------------------------
set.seed(7)
N   <- 10000
nr  <- 1000
nc  <- 10
A <- matrix(as.character(sample(1:2, N, replace = TRUE)), nr, nc)
B <- matrix(sample(letters[1:20], N, replace = TRUE), nr, nc)
matprR <- function(A) apply(A, 1, paste0, collapse = "")

microbenchmark(matprR(A), matpr(A))
microbenchmark(table(matprR(A)), table_(matpr(A)))
microbenchmark(table(matprR(B)), table_(matpr(B)))

x <- matprR(A); z <- table(x)
y <- matpr(A); q <- table_(y)
identical(x, y)
all(z, q)
setequal(names(z), names(q))


