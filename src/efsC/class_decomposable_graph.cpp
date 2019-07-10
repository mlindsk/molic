#include "class_decomposable_graph.h"

decomposable_graph::decomposable_graph(std::vector<std::string> nodes) {
  for (auto & e : nodes) {
    set s;
    g.insert( {e, s} );
  }
}
  
void decomposable_graph::set_nhood(std::string v, set &nh_v) {
  adj_list_element v_nh_v(v , nh_v);
  g.insert(v_nh_v);
}

set decomposable_graph::get_nhood(std::string v) {
  return g[v];
} 

void decomposable_graph::add_edge(const std::string v, const std::string u) {
  set *nh_v = &g[v];
  set *nh_u = &g[u];
  nh_v->insert(u);
  nh_u->insert(v);
}

void decomposable_graph::show() {
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
