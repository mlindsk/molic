#ifndef MISCUTILS_H
#define MISCUTILS_H

#include "molic_types.h"

bool any_true(std::vector<bool> &v);
RCM  pairwise_comb(VS x);
RIV  count_unique(VS  x); // std::map<std::string, int>
VS   matpr(Rcpp::CharacterMatrix A);
VS   dfs(Rcpp::List adjList, std::string root);

#endif
