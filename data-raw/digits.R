## code to prepare `digits` dataset goes here

d1 <- read.csv("../inst/extdata/optdigits.tes", header = FALSE)
d2 <- read.csv("../inst/extdata/optdigits.tra", header = FALSE)
digits <- rbind(d1, d2)
colnames(digits)[65] <- "class"
digits[] <- lapply(digits, as.character)
digits[, 1:(ncol(digits)-1)]   <- to_single_chars(digits[, 1:(ncol(digits)-1)])

## save("digits", file = "data/digits.RData")
## usethis::use_data("digits")
