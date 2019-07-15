#ifndef CLIQUE_GRAPH_H
#define CLIQUE_GRAPH_H

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>           // For: for_each
#include <unordered_set>       // For: "set" typedefinition

using set = std::unordered_set<std::string>;

class clique_graph {
  public:
  std::vector<set>               cl;
  std::vector<std::vector<bool>> cm;
  clique_graph() = default;
  clique_graph(std::vector<std::string> nodes);
  void delete_clique(int i);
  void insert_clique(int i, int j, set &clique);
  void set_neighbor(int i, int j, bool val);
  void show(bool cl_or_cm);  
};

#endif
