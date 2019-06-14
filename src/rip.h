#ifndef RIP_H
#define RIP_H

#include "set_ops.h"
#include "misc_utils.h"

Rcpp::List mcs(Rcpp::List & adj);
VVS        perfect_cliques(VVS & x);
Rcpp::List perfect_separators(VVS & x);
Rcpp::List rip(Rcpp::List & adj);

#endif
