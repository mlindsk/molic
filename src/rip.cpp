#include "set_ops.h"
#include "misc_utils.h"
#include <iostream>
#include <numeric>
#include <vector>
#include <string>
#include <algorithm>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]
using namespace Rcpp;
using VS  = std::vector<std::string>;
using VVS = std::vector<std::vector<std::string>>;

//' @export
// [[Rcpp::export]]
VS mcs(Rcpp::List adjList) {
  // -------------------------------------
  // In: adjList: Adjacency list representation of a decomposable undirected graph
  //
  // Out: A perfect numbering of the nodes
  // -------------------------------------
  // VS nodes = as<VS>(adjList.names());
  // Consider removing the VS nodes argument as just call names(adjList)
  VS  nodes = adjList.names();
  int N = nodes.size();
  // if( N - 1 == false ) return nodes[0];
  std::unordered_map<std::string, int> labels = {};
  for( int i = 0; i < N; i++ ) {
    labels.emplace(nodes[i], 0);
  }
  decltype(nodes) remaining_nodes = nodes;
  decltype(nodes) used_nodes(N, "");
  auto v = nodes[0];
  used_nodes[0] = v;
  remaining_nodes.erase(remaining_nodes.begin()+0);
  // Increment neighbor nodes with a one
  for( int i = 1; i < N; i++ ) {
    auto ne_i = as<VS>(adjList[v]);
    for (auto it = ne_i.begin(); it != ne_i.end(); ++it) {
      auto ne_ = labels.find(*it);
      ne_->second++;
    }
    std::string max_v;
    int max_val = -1;
    VS::iterator max_it;
    for (auto it = remaining_nodes.begin(); it != remaining_nodes.end(); ++it) {
      auto rn = labels.find(*it);
      int max_candidate = rn->second;
      if( max_candidate > max_val ) {
	max_v   = *it;
	max_val = max_candidate;
	max_it  = it;
      }
    }
    v = max_v;
    used_nodes[i] = v;
    remaining_nodes.erase(max_it);
  }
  return used_nodes;
} 

// [[Rcpp::export]]
VVS perfect_sequence(Rcpp::List adjList, VS z) {
  // z: mcs object
  //   - See Graphical Models, Lemma 2.14, by Steffen Lauritzen
  //   - for the correctness of this function.
  // Out: Perfect sequence of sets B_1, B2, ..., BK
  int n = z.size();
  VVS ps(n);
  for( int i = 0; i < n; i++ ) {
    // FIX!! DONT NEED TO CONVERT HERE!
    VS ne_i = as<VS>(adjList[z[i]]);
    ne_i.push_back(z[i]); // The closure of z_i
    VS zi_1 = VS(z.begin(), z.begin() + i + 1);
    VS si   = set_intersect(ne_i, zi_1);
    ps[i] = si;
  }
  return ps;
}

// [[Rcpp::export]]
VVS perfect_cliques(VVS & x) {
  // In: 
  // x: a perfect sequence of sets (denoted B_i in Lauritzen)
  //
  // Out:
  // y: a perfect sequence of the cliques
  int n = x.size();
  VVS pc;
  for(int i = 0; i < n; i++) {
    std::vector<bool> v; // Dummy var to loop over
    for(int j = 0; j < n; j++) {
      if( j != i ) {
	v.push_back(is_subseteq(x[i], x[j]));
      }
    }
    if( !any_true(v) ) {
      pc.push_back(x[i]); 
    }
  }
  return pc;
}

// [[Rcpp::export]]
Rcpp::List perfect_separators(VVS & x) {
  // x: Cliques (with RIP ordering)
  // S_j := H_{j-1} \cap C_j, H_{j-1} := \cap_k C_{k-1}, k = 1, 2, ..., j-1
  int n = x.size();
  Rcpp::List ps(n);       // All elements initialized to NULL
  if( n == 1 ) return ps; // List::create(_[""] = R_NilValue);
  for (int i = 1; i < n; i++) {
    VS Hi_1;
    for (int j = 0; j < i; j++) {
      Hi_1.insert(Hi_1.end(), x[j].begin(), x[j].end());
    }
    ps[i] = set_intersect(x[i], Hi_1);
  }
  return ps;
}

//' @export
// [[Rcpp::export]]
Rcpp::List rip(Rcpp::List & adjList) {
  VS z = mcs(adjList);
  VVS pseq = perfect_sequence(adjList, z);
  VVS pc = perfect_cliques(pseq);
  Rcpp::List ps = perfect_separators(pc);
  return List::create(_["C"] = pc , _["S"] = ps);
}
