#ifndef DECOMPOSABLE_GRAPH_H
#define DECOMPOSABLE_GRAPH_H

#include <iostream>
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>

using set              = std::unordered_set<std::string>;
using adj_list         = std::unordered_map<std::string, set>;
using adj_list_element = std::pair<std::string, set>;

class decomposable_graph {
private:
  adj_list g;

public:
  decomposable_graph() = default;
  decomposable_graph(std::vector<std::string> nodes);
  void set_nhood(std::string v, set &nh_v);
  set  get_nhood(std::string v);
  void add_edge(const std::string v, const std::string u);
  void show();
};

#endif
