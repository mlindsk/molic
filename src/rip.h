#ifndef RIP_H
#define RIP_H

#include "set_ops.h"
#include "misc_utils.h"

VS         mcs(Rcpp::List adj);
VVS        perfect_sequence(Rcpp::List adj, VS z);
VVS        perfect_cliques(VVS & x);
Rcpp::List perfect_separators(VVS & x);
Rcpp::List rip(Rcpp::List & adj);

#endif
