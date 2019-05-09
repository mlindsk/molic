#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "molic_types.h"

bool                       any_true(std::vector<bool> &v);
Rcpp::CharacterMatrix      pairwise_comb(VS x);
std::map<std::string, int> count_unique(VS  x);
VS                         matpr(Rcpp::CharacterMatrix A);
VS                         dfs(Rcpp::List adjList, std::string root);

#endif
