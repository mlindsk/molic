/*****************************************
                   TODO
 * Implement:
 * - as_mat
 * - as_adj
 ***************************************/

#include "misc_utils.h"

// [[Rcpp::export]]
bool any_true(std::vector<bool> &v) {
  return std::any_of(v.begin(), v.end(), [](bool i){ return i == true; });
}

// [[Rcpp::export]]
Rcpp::CharacterMatrix pairwise_comb(VS x) {
  /*****************************************
   * In:
   * - x: Vector of strings
   * Out: All pairwise combinations (i, j) for i != j
   ***************************************/
  const int N = x.size();
  const int  n_complete = N * (N - 1) / 2;
  Rcpp::CharacterMatrix A(n_complete, 2);
  int row = 0;
  for( int i = 0; i < N; ++i ) {
    for( int j = i+1; j < N; ++j ) {
      A(row, 0) = x[i];
      A(row, 1) = x[j];
      row++;
    }
  }
  return A;
}

// [[Rcpp::export]]
std::map<std::string, int> count_unique(VS  x) {
  /*****************************************
   * In:
   * - x: Vector of strings
   * Out: A namedvector of counts of all unique
   *      elements in x
   ***************************************/
  std::map<std::string, int> tab;
  int n = x.size();
  for (int i = 0; i < n; i++) {
    auto s = x[i];
    tab[s]++;
  }
  return tab;
}

// [[Rcpp::export]]
VS matpr(Rcpp::CharacterMatrix A) {
  /*****************************************
   * In:
   * - A:
   * Out: A vector of length nrow(A) with i'th
   *      element being a concatenation of all
   *      cells in row i
   ***************************************/
  int n = A.nrow();
  VS  x(n);
  for( int i = 0; i < n; i++ ) {
    auto row = A(i, _);
    std::string s;
    s = std::accumulate(row.begin(),row.end(), s);
    x[i] = s;
  }
  return x;
}

// [[Rcpp::export]]
VS dfs(Rcpp::List adj, std::string root) {
  /*****************************************
   * In:
   * - adj: A named adjacency list
   * - root: The rooth node
   * Out: All nodes connected to the root
   ***************************************/
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
