#ifndef UNORDERD_SET_OPS_H
#define UNORDERD_SET_OPS_H

#include <iostream>
#include <algorithm>     // For: set operations/algs
#include <vector>
#include <string>
#include <unordered_set>

using set = std::unordered_set<std::string>;

set  set_intersect(set &v1, set &v2);
set  set_union(set &v1, set &v2);
set  set_diff(set &v1, set &v2);
bool set_equal(set &v1, set &v2);
bool set_in(const std::string & a, const set & b);
bool set_issubeq(set & a, set & b);

#endif
