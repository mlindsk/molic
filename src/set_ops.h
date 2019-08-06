#ifndef SETOPS_H
#define SETOPS_H

#include "molic_types.h"

// #include <vector>
// #include <string>
// #include <unordered_set>
// // [[Rcpp::plugins(cpp11)]]

// using set = std::unordered_set<std::string>;
// using VS  = std::vector<std::string>;

/**
 * For efs c++ version
 */

// set  set_intersect(set &v1, set &v2);
// set  set_union(set &v1, set &v2);
// set  set_diff(set &v1, set &v2);
// bool set_equal(set &v1, set &v2);
// bool set_in(const std::string & a, const set & b);
// bool set_issubeq(set & a, set & b);

/**
 * For outlier_utils
 */

VS   set_intersect(VS &v1, VS &v2);
// VS   set_union(VS &v1, VS &v2);
// VS   set_diff(VS &v1, VS &v2);
// bool set_eq(VS &v1, VS &v2);
bool set_in(std::string & a, VS &b);
bool set_issubeq(VS &a, VS &b);
bool set_any(std::vector<bool> &v);

#endif
