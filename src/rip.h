#ifndef RIP_H
#define RIP_H

#include <Rcpp.h>

using VVS = std::vector<std::vector<std::string>>;

Rcpp::List mcs(Rcpp::List & adj);
VVS        perfect_cliques(VVS & x);
Rcpp::List perfect_separators(VVS & x);
Rcpp::List rip(Rcpp::List & adj);

#endif
