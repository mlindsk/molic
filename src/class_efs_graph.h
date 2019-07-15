#ifndef EFS_GRAPH_H
#define EFS_GRAPH_H

#include "class_decomposable_graph.h"
#include "class_clique_graph.h"
#include <unordered_set>
#include <unordered_map>
#include <utility>
#include <vector>
#include <string>
#include <Rcpp.h>

using RCM             = Rcpp::CharacterMatrix;
using set             = std::unordered_set<std::string>;
using entropy_edges   = std::unordered_map<std::string, double>;
using entropy_memoizer= std::unordered_map<std::string, double>;

struct msi_obj {
  set S;
  set C1;
  set C2;
  entropy_edges edges;
};

struct max_obj {
  std::pair<std::string, double> max_val;
  int                            max_MSI_pos;
  std::pair<int, int>            max_CG_pos;
  void show();
};

class efs_graph {
public:
  decomposable_graph   G;
  clique_graph         CG;
  std::vector<msi_obj> MSI;
  max_obj              MAX;
  entropy_memoizer     EM;
  efs_graph() = default;
  efs_graph(RCM &dm);
};


#endif
