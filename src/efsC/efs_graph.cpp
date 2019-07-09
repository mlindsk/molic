// https://codereview.stackexchange.com/questions/175104/adjacency-list-graph-representation
// https://stackoverflow.com/questions/14133115/implementation-of-an-adjacency-list-graph-representation
// https://github.com/petewerner/misc/wiki/RcppArmadillo-cheatsheet

#include <iostream>
#include <algorithm>
#include <utility>
#include <vector>
#include <string>
#include <unordered_set>
#include <unordered_map>
#include "entropy.h"
#include "efs_graph.h"

pairwise_node_list pairwise_comb(std::vector<std::string> &x) {
  int n = x.size();
  int n_complete = n * (n - 1) / 2;
  pairwise_node_list A(n_complete, std::make_pair("", ""));
  int row = 0;
  for( int i = 0; i < n; ++i ) {
    for( int j = i+1; j < n; ++j ) {
      A[row].first  = x[i];
      A[row].second = x[j];
      row++;
    }
  }
  return A;
}

class decomposable_graph {
  private:
  adj_list g;

  public:
  decomposable_graph() = default;
  decomposable_graph(std::vector<std::string> nodes) {
    for (auto & e : nodes) {
      set s;
      g.insert( {e, s} );
    }
  }
  
  void set_nhood(std::string v, set &nh_v) {
    adj_list_element v_nh_v(v , nh_v);
    g.insert(v_nh_v);
  }

  set get_nhood(std::string v) {
    return g[v];
  } 

  void add_edge(const std::string v, const std::string u) {
    set *nh_v = &g[v];
    set *nh_u = &g[u];
    nh_v->insert(u);
    nh_u->insert(v);
  }

  void show() {
    for (auto & e : g) {
      std::string key = e.first;
      set val = e.second;
      std::cout << key << " : ";
      for (auto & q : val) {
	std::cout << q << " ";
      }
      std::cout << "\n";
    }
  }

};

class clique_graph {
  public:
  clique_list cl;
  clique_matrix cm;

  clique_graph() = default;
  clique_graph(std::vector<std::string> nodes) : cm(nodes.size()) {
    int n = nodes.size();
    for (int i = 0; i < n; i++) {
      set se = {nodes[i]};
      cl.push_back(se);
      cm[i] = std::vector<bool>(n, true);
      cm[i][i] = false;
    }
  }
  
  void delete_clique(int i) {
    cl.erase(cl.begin() + i); // i = 0 erases the first element
    cm.erase(cm.begin() + i);
    std::for_each(std::begin(cm),
		  std::end(cm),
		  [&](std::vector<bool> &x) {
		    x.erase(x.begin() + i);
    });
  }

  void insert_clique(int i, int j, set &clique) { // Insert and let clique be adjacent to clique i and clique j
    cl.push_back(clique);
    int n = cm.size();
    std::vector<bool> last_row(n + 1, false);
    cm.push_back(last_row);
    cm[n][i] = true;
    cm[n][j] = true;
    for (int k = 0; k < n + 1; k++) {
      bool is_ne = (k == i) || (k == j);
      if( is_ne ) cm[k].push_back(true);
        else cm[k].push_back(false);
    }
  }

  void set_neighbor(int i, int j, bool val) {
    cm[i][j] = val;
    cm[j][i] = val;
  }

  void show(bool cl_or_cm = true) { // cl = true
    if ( cl_or_cm ) {
      for (int i = 0; i < cl.size(); i++) {
	set cl_i = cl[i];
	std::cout << i << " : ";
	for (auto & e : cl_i) {
	  std::cout << e << " ";
	}
	std::cout << "\n";
      }
    }
    else {
      int n = cm.size();
      for (int i = 0; i < n; i++) {
	for (int j = 0; j < n; j++) {
	  std::cout << cm[i][j] << " ";
	}
	std::cout << "\n";
      }
    }
  }
  
};

struct msi_obj {
  set S;
  set C1;
  set C2;
  entropy_edges edges;
};

struct max_obj {
  max_edge_value       max_val;
  max_edge_pos_in_msi  max_MSI_pos;
  max_C1_C2_pos_in_CG  max_CG_pos;  
};

struct data_matrix {
  matrix mat;
  std::vector<std::string> col_names;

  void show(int nrow, int ncol) {
    for (int i = 0; i < ncol; i++) {
      std::cout << col_names[i] << " ";
    }
    std::cout << "\n";
    for (int i = 0; i < nrow; i++) {
      for (int j = 0; j < ncol; j++) {
	std::cout << mat[i][j] << " ";
      }
      std::cout << "\n";
    }
  }
  
};

class efs_graph {
public:
  decomposable_graph   G;
  clique_graph         CG;
  std::vector<msi_obj> MSI;
  max_obj              MAX;
  entropy_memoizer     EM;
  // public:
  efs_graph(void);
  efs_graph(data_matrix &dm);
};



efs_graph::efs_graph(data_matrix &dm) : G(dm.col_names), CG(dm.col_names)
{
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
  data_matrix dm;
  matrix m = {
	      {"a", "a", "c"},
	      {"a", "c", "t"},
	      {"c", "a", "a"}
  };
  std::vector<std::string> cn({"z", "x", "y"});
  dm.mat = m;
  dm.col_names = cn;
  efs_graph eg(dm);
  // auto em = eg.EM;
  // dm.show(m.size(), cn.size());
  // eg.CG.show();
  // pairwise_node_list pnl = pairwise_comb(cn);
  // for (int i = 0; i < pnl.size(); i++) {
  //   std::cout << pnl[i].first <<  " - " << pnl[i].second << "\n";
  // }
  // decomposable_graph G(std::vector<std::string>{"a", "b", "c", "d", "e", "f"});
  // clique_graph CG(std::vector<std::string>{"a", "b", "c", "d", "e", "f"});
  // G.add_edge("b", "c");
  // G.show();
  // CG.set_neighbor(3, 1, false);
  // CG.delete_clique(3);
  // CG.delete_clique(2);
  // CG.show(false);
  // set ss = {"c", "d", "e"};
  // CG.insert_clique(1, 3, ss);
  // std::cout << "\n";
  // CG.show(false);

  // clique_graph cl;
  // cl.insert_clique(1, 0, an);
  // cl.insert_clique(0, 1, an);
  // cl.delete_clique(0);
  // cl.show(false);

  // msi_obj msi;
  // auto S = msi.S = {"a", "b", "c", "d", "e", "f"};
  // std::cout << S.empty() << "\n";
  
  // auto &f = msi.S["a"];
  // std::cout << f << "\n";
  // std::cout << msi.S.begin() == msi.S.end() << "\n";
  
  return 0;
}
