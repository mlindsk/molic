#ifndef MISCUTILS_H
#define MISCUTILS_H

#include <numeric>
#include <vector>
#include <string>
#include <algorithm>
#include <stack>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using VS  = std::vector<std::string>;
using VVS = std::vector<std::vector<std::string>>;

bool any_true(std::vector<bool> &v);
Rcpp::CharacterMatrix pairwise_comb(VS x);
std::map<std::string, int> table_count(VS  x);
VS matpr(Rcpp::CharacterMatrix A);
VS dfs(Rcpp::List adjList, std::string root);

#endif
