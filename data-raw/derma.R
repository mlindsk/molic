library(dplyr)

derma <- read.table(file = "../inst/extdata/dermatology.data", sep = ",") %>%
  as_tibble() %>%
  filter(V34 != "?") %>%                 # V34 = age
  mutate(V34 = cut(as.numeric(V34), 6, labels = c(1:6))) %>%
  mutate_all(as.character) %>%
  setNames(c(paste("c", 1:11, sep = ""), # 
    paste("h", 12:33, sep = ""),         # 
    "age",
    "ES"))                               # Class variables

derma <- derma %>%
  mutate(ES = case_when(
    ES == "1" ~ "psoriasis",
    ES == "2" ~ "seboreic dermatitis",
    ES == "3" ~ "lichen planus",
    ES == "4" ~ "pityriasis rosea",
    ES == "5" ~ "chronic dermatitis",
    ES == "6" ~ "pityriasis rubra pilaris",
    )
  )

usethis::use_data(derma)
