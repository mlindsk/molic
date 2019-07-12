#ifndef EFS_UTILS_H
#define EFS_UTILS_H

#include <vector>     // For: 
#include <string>     // For:
#include <utility>

using vec_str_pair = std::vector<std::pair<std::string, std::string>>;

vec_str_pair pairwise_comb(std::vector<std::string> &x);

#endif
