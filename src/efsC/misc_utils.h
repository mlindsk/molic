#ifndef MISC_UTILS_H
#define MISC_UTILS_H

#include <vector>
#include <string>

using string_pair        = std::pair<std::string, std::string>;
using pairwise_node_list = std::vector<string_pair>;

pairwise_node_list pairwise_comb(std::vector<std::string> &x);

#endif
