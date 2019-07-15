library(Rcpp)
library(molic)
library(dplyr)
sourceCpp("rcpp_wrappers.cpp")

A <- matrix(letters[1:9], ncol = 3, dimnames = list(NULL, c("x", "y", "z")))
B <- matrix(letters[1:9], ncol = 3)
showme(A)
showme(B)
df <- tgp_dat[1:1000, 5:100]
S <- as.matrix(df)

mainR(S)
