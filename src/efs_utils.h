#ifndef EFS_UTILS_H
#define EFS_UTILS_H

#include <unordered_map>
#include <vector>     // For: 
#include <string>     // For:
#include <utility>
#include <Rcpp.h>

using RCM          = Rcpp::CharacterMatrix;
using vec_str_pair = std::vector<std::pair<std::string, std::string>>;

vec_str_pair pairwise_comb(std::vector<std::string> &x);
RCM sub_dm( RCM & A, std::vector<std::string> & x );
double entropy_(RCM & dm);
// table_of_counts count_unique(std::vector<std::string> &x);
// std::vector<std::string> paste_rows(row_matrix &m, std::vector<int> d);
// double row_entropy(row_matrix &m, std::vector<int> d);
// double col_entropy(col_matrix &m, int i);

#endif
