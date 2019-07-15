devtools::load_all()
suppressMessages(library(dplyr))

df   <- molic::tgp_dat %>%
  filter(pop_meta == "EUR") %>%
  select(-c("sample_name", "pop_meta"))
  ## select(1:100)
  ## slice(1:1000)

cat("C++ version")

A <- as.matrix(df)
t <- Sys.time()
mainC(A)
Sys.time() - t 

cat("R version")

t <- Sys.time()
E <- molic:::efs_init(df)
Sys.time() - t
print(length(E$ht))
print(E$ht[["rs11123719"]])
