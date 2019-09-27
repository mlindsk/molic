library(dplyr)
library(readr)
library(tidyr)

df_meta_haplo <- read_csv("inst/extdata/tgp_meta_haps.csv")[, c(1,3:6)]
colnames(df_meta_haplo) <- c("haplotype",
  "snps",
  "number_of_snps",
  "chrom",
  "snp_pos"
)

df_meta_haplo <- df_meta_haplo %>%
  drop_na()

df_meta_haplo <- df_meta_haplo %>%
  mutate(haplotype = stringr::str_replace(haplotype, "-", ""))

# save(df_meta_haplo, file = "../data/df_meta_haplo.Rdat")

df_meta_pop <- read_tsv("inst/extdata/tgp_meta_pop.tsv")
colnames(df_meta_pop) <- c("sample_name",
  "sex",
  "bio_id",
  "pop_sub",
  "pop_sub_",
  "pop_meta",
  "pop_meta_",
  "collection_method"
)

# save(df_meta_pop, file = "../data/df_meta_pop.Rdat")

df <- read_delim("inst/extdata/tgp_dat", ",", col_names = T) %>%
  gather(sample_name, snp, -X1) %>%
  spread(X1, snp)

df <- df %>%
  left_join(df_meta_pop[, c("sample_name", "pop_meta")], by = c("sample_name")) %>%
  select(sample_name, pop_meta, everything())


df1 <- df %>%
  mutate_at(vars(contains("rs")), funs(stringr::str_sub(., 1, 1))) %>%
  mutate(sample_name = paste0(sample_name, "a"))

df2 <- df %>%
  mutate_at(vars(contains("rs")), funs(stringr::str_sub(., 3, 3))) %>%
  mutate(sample_name = paste0(sample_name, "b"))

df <- rbind(df1, df2)

haplotypes <- lapply(pull(df_meta_haplo, snps), function(x) {
  unlist(stringr::str_split(x, "/"))
})

names(haplotypes) <- pull(df_meta_haplo, haplotype)

# Drop haplotypes for which the snps are not present in df
snps <- colnames(df[, 3:ncol(df)])
haplotypes <-lapply(haplotypes, function(x) {
  if( !all( x %in% snps )) return(NULL)
  x
})

filter_null <- function(x) Filter(function(f) !is.null(f), x)
haplotypes <- filter_null(haplotypes)
df <- df[, c("sample_name", "pop_meta", unlist(haplotypes))]

# REMOVE THOSE SNPs WITH ONLY A SINGLE ALLELE OCCURING:
# Ex: for EUR rs150209521 has only T

df_meta_split <- split(df, df$pop_meta)
snps_to_keep <- lapply(df_meta_split, function(x) {
  out <- lapply(x[, 3:ncol(x)], function(y) {
    length(unique(y)) > 1
  })
  names(Filter(all, out))
})

snps_to_keep <- Reduce(intersect, snps_to_keep)
df <- df[, c("sample_name", "pop_meta", snps_to_keep)]

reduced_haplo <- names(Filter(all, lapply(haplotypes, function(x) {
  all(x %in% colnames(df))
})))

haplotypes <- haplotypes[reduced_haplo]
df <- df[, c("sample_name", "pop_meta", unlist(haplotypes))]

# Save
tgp_dat  <- df
tgp_haps <- haplotypes

## save(tgp_dat,  file = "../data/tgp_dat.RData")
## save(tgp_haps, file = "../data/tgp_haps.RData")
