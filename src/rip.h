#ifndef RIP_H
#define RIP_H

#include "molic_types.h"
#include "set_ops.h"     // For set_any

// #include <Rcpp.h>
// #include <vector>
// // [[Rcpp::plugins(cpp11)]]

using VVS = std::vector<std::vector<std::string>>;

Rcpp::List mcs(Rcpp::List & adj);
VVS        perfect_cliques(VVS & x);
Rcpp::List perfect_separators(VVS & x);
Rcpp::List rip(Rcpp::List & adj);

#endif
