#' A data frame with genetic data from the 1000 genomes project
#'
#' The data consists of \code{2504} DNA profiles, each genotyped on 304 SNPs (binary variables).
#' The data frame has \code{5008} rows, since each profile has two copies.
#' 
#' @docType data
#' 
#' @references \href{https://www.ncbi.nlm.nih.gov/pubmed/26432245}{1000 Genomes Project}
"tgp_dat"


#' A named list of character vectors.
#'
#' Every element in the list is a character vector that forms a haplotype
#' from the 1000 genomes project. If the list is unlisted, it should correspond
#' to colnames of tgp_dat. In other words, tgp_haps is a "haplotype-grouping"
#' of the variables in tgp_dat.
#' 
#' @docType data
#' 
#' @references \href{https://www.ncbi.nlm.nih.gov/pubmed/26432245}{1000 Genomes Project}
"tgp_haps"


#' A data frame with meta information of the data in \code{tgp_haps}
#'
#' The data frame holds information about haplotypes and on which chromosome
#' these are located.
#' 
#' @docType data
#' 
#' @references \href{https://www.ncbi.nlm.nih.gov/pubmed/26432245}{1000 Genomes Project}
"tgp_meta"
