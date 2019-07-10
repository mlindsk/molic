#include "molic_types.h"

efs_graph::efs_graph(data_matrix &dm) : G(dm.col_names), CG(dm.col_names) {
  std::vector<std::string> nodes = dm.col_names;
  int n = nodes.size();
  pairwise_node_list pairs = pairwise_comb(nodes);

  for (auto & e : pairs) {
    std::cout << e.first << " - " << e.second << "\n";
  }

  for (int i = 0; i < n; i++) {
    double ent_i = entropy(dm.mat, std::vector<int>{i});
    EM[nodes[i]] = ent_i;
  }

  double max_entropy;
  
  for (int i = 0; i < pairs.size(); i++) {
    std::string node_1 = pairs[i].first;
    std::string node_2 = pairs[i].second;
    if ( node_2 < node_1 ) std::swap(node_1, node_2); // Sort
    std::string edge_i = node_1 + "|" + node_2;
    std::cout << edge_i << "\n";
    EM[edge_i] = entropy(dm.mat, std::vector<int>({i}));
    double entropy_i = EM[node_1] + EM[node_2] - EM[edge_i];
    
    if ( entropy_i > max_entropy) {
      max_entropy = entropy_i;
      MAX.max_val     = std::make_pair(edge_i, entropy_i);
      MAX.max_MSI_pos = i;

    }
    
    msi_obj msi;
    msi.C1    = set({node_1});
    msi.C2    = set({node_2});
    msi.edges[edge_i] = entropy_i; 
    MSI.push_back(msi);
    /*----------------------------------------------------------*
     *                     BOOKMARK                                   
     * ---------------------------------------------------------*/
    // Find CG position:
    // for (int i = 0; i < n; i++) {
    //   // TBA
    // }
    // auto C1_pos = std::find(CG.cl.begin(), CG.cl.end(), MSI[MAX.max_MSI_pos].C1);
    // auto C2_pos = std::find(CG.cl.begin(), CG.cl.end(), MSI[MAX.max_MSI_pos].C2);
    // Maybe use set_eual here?
    // std::cout << C1_pos == CG.cl.end() << "\n";
    // MAX.max_CG_pos  = std::make_pair(*C1_pos, *C2_pos);
    
  }
    
}

int main() {
  return 0;
}

// int main() {
//   return 0;
// }
