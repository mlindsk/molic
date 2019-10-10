## code to prepare `digits` dataset goes here

d1 <- read.csv("../inst/extdata/optdigits.tes", header = FALSE)
d2 <- read.csv("../inst/extdata/optdigits.tra", header = FALSE)
digits <- rbind(d1, d2)
colnames(digits)[65] <- "class"
digits[] <- lapply(digits, as.character)
digits[, 1:(ncol(digits)-1)]   <- molic::to_single_chars(digits[, 1:(ncol(digits)-1)])

# save("digits", file = "../data/digits.Rdata", version = 2)
# saveRDS("digits", file = "../data/digits.rds", version = 2)
usethis::use_data(digits)
