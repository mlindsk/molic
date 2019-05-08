#ifndef RIP_H
#define RIP_H

#include "set_ops.h"
#include "misc_utils.h"
#include <iostream>
#include <numeric>
#include <vector>
#include <string>
#include <algorithm>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using VS  = std::vector<std::string>;
using VVS = std::vector<std::vector<std::string>>;

VS mcs(Rcpp::List adjList);
VVS perfect_sequence(Rcpp::List adjList, VS z);
VVS perfect_cliques(VVS & x);
Rcpp::List perfect_separators(VVS & x);
Rcpp::List rip(Rcpp::List & adjList);

#endif
