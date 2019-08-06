#include "molic_types.h"
// #include <Rcpp.h>
// #include <vector>
// #include <string>
// #include <unordered_map>
// #include <stack>
// // [[Rcpp::plugins(cpp11)]]
// using VS  = std::vector<std::string>;

//' Depth First Search
//'
//' Finds the elements in the component of \code{root}
//' 
//' @param adj A named adjacency list of a decomposable grah
//' @param root The node from which the component should be found
//' @return All nodes connected to \code{root}
//' @export
// [[Rcpp::export]]
VS dfs(Rcpp::List adj, std::string root) {
  VS nodes = adj.names();
  int n = nodes.size();
  std::unordered_map<std::string, bool> visited;
  std::vector<std::string> connected_to_root;
  for( int i = 0; i < n; i++ ) {
    visited.emplace(nodes[i], false); 
  }
  std::stack<std::string> S;
  S.push(root);
  while( !S.empty() ) {
    std::string u = S.top();
    S.pop();
    if( !visited[u] ) {
      visited[u] = true;
      connected_to_root.push_back(u);
      VS adj_u = adj[u];
      for ( auto & w : adj_u ) {
      	if( !visited[w] ) {
      	  S.push(w);
      	}
      }
    }
  }
  return connected_to_root;
}
