#ifndef EFS_GRAPH_H
#define EFS_GRAPH_H

using set                 = std::unordered_set<std::string>;
using entropy_edges       = std::unordered_map<std::string, double>;
using clique_matrix       = std::vector<std::vector<bool>>;
using clique_list         = std::vector<set>;
using adj_list            = std::unordered_map<std::string, set>;
using adj_list_element    = std::pair<std::string, set>;
using max_edge_pos_in_msi = int;
using max_C1_C2_pos_in_CG = std::pair<int, int>;
using max_edge_value      = std::pair<std::string, double>;
using entropy_memoizer    = std::unordered_map<std::string, double>;
using matrix              = std::vector<std::vector<std::string>>;
using string_pair         = std::pair<std::string, std::string>;
using pairwise_node_list  = std::vector<string_pair>;

pairwise_node_list pairwise_comb(std::vector<std::string> &x);

#endif
