#ifndef EFS_GRAPH_H
#define EFS_GRAPH_H

using set           = std::unordered_set<std::string>;
using entropy_edges = std::unordered_map<std::string, double>;

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
};

class efs_graph {
public:
  decomposable_graph   G;
  clique_graph         CG;
  std::vector<msi_obj> MSI;
  max_obj              MAX;
  entropy_memoizer     EM;
  // efs_graph(void);
  efs_graph(data_matrix &dm);
};


#endif
