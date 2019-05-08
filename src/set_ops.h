#ifndef SETOPS_H
#define SETOPS_H

#include <vector>
#include <string>
#include <algorithm>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

using VS  = std::vector<std::string>;

VS set_intersect(VS &v1, VS &v2);
VS set_union(VS &v1, VS &v2);
VS set_diff(VS &v1, VS &v2);
bool set_eq(VS &v1, VS &v2);
bool is_subseteq(VS const& a, VS const& b);

#endif
