#ifndef MOLIC_TYPES_H
#define MOLIC_TYPES_H

#include <iostream>
#include <algorithm>     // For: set operations
#include <utility>       // For: swap
#include <cmath>         // For: log()
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>

using set                 = std::unordered_set<std::string>;
using matrix              = std::vector<std::vector<std::string>>;
using table               = std::unordered_map<std::string, int>;
using entropy_edges       = std::unordered_map<std::string, double>;
using clique_list         = std::vector<set>;
using clique_matrix       = std::vector<std::vector<bool>>;
using adj_list            = std::unordered_map<std::string, set>;
using adj_list_element    = std::pair<std::string, set>;
using entropy_memoizer    = std::unordered_map<std::string, double>;
using string_pair         = std::pair<std::string, std::string>;
using pairwise_node_list  = std::vector<string_pair>;

#include "class_decomposable_graph.h"
#include "class_clique_graph.h"
#include "class_data_matrix.h"
#include "class_efs_graph.h"
#include "misc_utils.h"
#include "entropy.h"

#endif
