#ifndef SETOPS_H
#define SETOPS_H

#include "molic_types.h"

VS   set_intersect(VS &v1, VS &v2);
VS   set_union(VS &v1, VS &v2);
VS   set_diff(VS &v1, VS &v2);
bool set_eq(VS &v1, VS &v2);
bool is_element_present(std::string & a, VS & b);
bool is_subseteq(VS const& a, VS const& b);

#endif
