#include "class_efs_graph.h"
#include "set_ops.h"
#include "efs_utils.h"
#include "outlier_utils.h"
#include <algorithm>
#include <Rcpp.h>

using str_pair     = std::pair<std::string, std::string>;
using vec_str_pair = std::vector<str_pair>;

void max_obj::show() {
  Rcpp::Rcout << max_val.first << " : " << max_val.second << "\n";
}

efs_graph::efs_graph(RCM &dm) : G(Rcpp::as<std::vector<std::string>>(Rcpp::colnames(dm))),
				CG(Rcpp::as<std::vector<std::string>>(Rcpp::colnames(dm))) {
  std::vector<std::string> nodes = Rcpp::as<std::vector<std::string>>(Rcpp::colnames(dm));
  int n = nodes.size();

  // MAKE subM take a std::vector<std::string> input instead of RCV
  
  for (int i = 0; i < n; i++) {
    std::string node_i = nodes[i];
    std::vector<std::string>  node_i_as_vec = {node_i};
    RCM sub_dm_i = sub_dm(dm, node_i_as_vec);
    double ent_i  = entropy(sub_dm_i);
    EM[node_i]  = ent_i;
  }

  double max_entropy;
  vec_str_pair pairs = pairwise_comb(nodes);
  int np = pairs.size();
  MSI.resize(np);
  
  for (int i = 0; i < np; i++) {

    std::string node_1 = pairs[i].first;
    std::string node_2 = pairs[i].second;
    if ( node_2 < node_1 ) std::swap(node_1, node_2); // Sort
    std::string edge_i = node_1 + "|" + node_2;

    /*----------------------------------------------------------*
     *                       FIX                                   
     * ---------------------------------------------------------*/
    std::vector<std::string>  pair_as_vec = {node_1, node_2};
    RCM sub_dm_pair = sub_dm(dm, pair_as_vec);
    EM[edge_i] = entropy(sub_dm_pair);
    double entropy_i = EM[node_1] + EM[node_2] - EM[edge_i];
    /* ---------------------------------------------------------*/
    
    if ( entropy_i > max_entropy) {
      max_entropy     = entropy_i;
      MAX.max_val     = std::make_pair(edge_i, entropy_i);
      MAX.max_MSI_pos = i;
    }
    
    msi_obj msi;
    msi.C1    = set({node_1});
    msi.C2    = set({node_2});
    msi.edges[edge_i] = entropy_i; 
    MSI[i] = msi;

  }

  int C1_cg_pos;
  set &C1 = MSI[MAX.max_MSI_pos].C1;
  int C2_cg_pos;
  set &C2 = MSI[MAX.max_MSI_pos].C2;
  
  int n_cl = CG.cl.size(); 
  for (int i = 0; i < n_cl; i++) {
    if (set_equal(C1, CG.cl[i])) C1_cg_pos = i;
    if (set_equal(C2, CG.cl[i])) C2_cg_pos = i;
  }
  
  MAX.max_CG_pos  = std::make_pair(C1_cg_pos, C2_cg_pos);
  Rcpp::Rcout << EM.size() << "\n";
  Rcpp::Rcout << EM["rs11123719"] << "\n";
  
}
