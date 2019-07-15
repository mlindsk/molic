#include "class_clique_graph.h"
#include <Rcpp.h>
#include <algorithm>           // For: for_each

clique_graph::clique_graph(std::vector<std::string> nodes) : cm(nodes.size()) {
  int n = nodes.size();
  for (int i = 0; i < n; i++) {
    set se = {nodes[i]};
    cl.push_back(se);
    cm[i] = std::vector<bool>(n, true);
    cm[i][i] = false;
  }
}
  
void clique_graph::delete_clique(int i) {
  cl.erase(cl.begin() + i); // i = 0 erases the first element
  cm.erase(cm.begin() + i);
  std::for_each(std::begin(cm),
		std::end(cm),
		[&](std::vector<bool> &x) {
		  x.erase(x.begin() + i);
		});
}

void clique_graph::insert_clique(int i, int j, set &clique) { // Insert and let clique be adjacent to clique i and clique j
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

void clique_graph::set_neighbor(int i, int j, bool val) {
  cm[i][j] = val;
  cm[j][i] = val;
}

void clique_graph::show(bool cl_or_cm = true) { // cl = true
  if ( cl_or_cm ) {
    int n = cl.size();
    for (int i = 0; i < n; i++) {
      set cl_i = cl[i];
      Rcpp::Rcout << i << " : ";
      for (auto & e : cl_i) {
	Rcpp::Rcout << e << " ";
      }
      Rcpp::Rcout << "\n";
    }
  }
  else {
    int n = cm.size();
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
	Rcpp::Rcout << cm[i][j] << " ";
      }
      Rcpp::Rcout << "\n";
    }
  }
}
