// Vector ops: https://www.geeksforgeeks.org/vector-in-cpp-stl/
// Set ops: https://lemire.me/blog/2017/01/27/how-expensive-are-the-union-and-intersection-of-two-unordered_set-in-c/

#include <iostream>
#include <numeric>  // For accumulate
#include <vector>
#include <string>
#include <algorithm>
// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
// include <Rcpp.h>

using namespace Rcpp;


// ------------------------------------------------------------------
// THESE SET OPERATIONS A TEMPORARY - THEY ARE SLOW BECAUSE OF "sort"
// ------------------------------------------------------------------

// [[Rcpp::export]]
std::vector<std::string> set_intersect(std::vector<std::string> &v1,
                                      std::vector<std::string> &v2){
    std::vector<std::string> v3;
    std::sort(v1.begin(), v1.end());
    std::sort(v2.begin(), v2.end());
    std::set_intersection(v1.begin(),v1.end(),
                          v2.begin(),v2.end(),
                          back_inserter(v3));
    return v3;
}

// [[Rcpp::export]]
std::vector<std::string> set_union(std::vector<std::string> &v1,
                                      std::vector<std::string> &v2){
    std::vector<std::string> v3;
    std::sort(v1.begin(), v1.end());
    std::sort(v2.begin(), v2.end());
    std::set_union(v1.begin(),v1.end(),
                          v2.begin(),v2.end(),
                          back_inserter(v3));
    return v3;
}

// [[Rcpp::export]]
std::vector<std::string> set_diff(std::vector<std::string> &v1,
				  std::vector<std::string> &v2){
    std::vector<std::string> v3;
    std::sort(v1.begin(), v1.end());
    std::sort(v2.begin(), v2.end());
    std::set_difference(v1.begin(),v1.end(),
                          v2.begin(),v2.end(),
                          back_inserter(v3));
    return v3;
}

// [[Rcpp::export]]
bool set_eq(std::vector<std::string> &v1,
	      std::vector<std::string> &v2) {
  if(v1.size() != v2.size()) return false;
  std::sort(std::begin(v1), std::end(v1));
  std::sort(std::begin(v2), std::end(v2));
  return std::equal(std::begin(v1), std::end(v1), std::begin(v2));
}

// [[Rcpp::export]]
std::vector<std::string> mcs_(List adjList) {
  // -------------------------------------
  // In: adjList: Adjacency list representation of a decomposable undirected graph
  //
  // Out: A perfect numbering of the nodes
  // -------------------------------------
  std::vector<std::string> nodes = as<std::vector<std::string>>(adjList.names());
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
    auto ne_i = as<std::vector<std::string>>(adjList[v]);
    for (auto it = ne_i.begin(); it != ne_i.end(); ++it) {
      auto ne_ = labels.find(*it);
      ne_->second++;
    }
    std::string max_v;
    int max_val = -1;
    std::vector<std::string>::iterator max_it;
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
std::map<std::string, int> table_(std::vector<std::string>  x) {
  std::map<std::string, int> tab;
  int n = x.size();
  for (int i = 0; i < n; i++) {
    auto s = x[i];
    tab[s]++;
  }
  return tab;
}

// [[Rcpp::export]]
std::vector<std::string> matpr(CharacterMatrix A) {
  // Concatenate rows in a character matrix
  int n = A.nrow();
  std::vector<std::string>  x(n);
  for( int i = 0; i < n; i++ ) {
    auto row = A(i, _);
    std::string s;
    s = std::accumulate(row.begin(),row.end(), s);
    x[i] = s;
  }
  return x;
}

// [[Rcpp::export]]
std::vector<std::vector<std::string>> perfect_seq(List adjList, std::vector<std::string> z) {
  // z: mcs object
  //   - See Graphical Models, Lemma 2.14, by Steffen Lauritzen
  //   - for the correctness of this function.
  // Out: Perfect sequence of sets B_1, B2, ..., BK
  int n = z.size();
  std::vector<std::vector<std::string>> ps(n);
  for( int i = 0; i < n; i++ ) {
    std::vector<std::string> ne_i = as<std::vector<std::string>>(adjList[z[i]]);
    std::vector<std::string> zi_1 = std::vector<std::string>(z.begin(), z.begin() + i + 1);
    // The set_intersect is slow because of sorting; Rs intersect is actually faster
    std::vector<std::string> si   = set_intersect(ne_i, zi_1);
    si.insert(si.begin(), z[i]);
    ps[i] = si;
  }
  return ps;
}

// [[Rcpp::export]]
bool is_subseteq(std::vector<std::string> const& a, std::vector<std::string> const& b) {
   for(auto const & av:a){
     if( std::find(b.begin(),b.end(),av) == b.end() )
       return false;
   }
   return true;
}

// [[Rcpp::export]]
bool any_true(std::vector<bool> &v) {
    return std::any_of(v.begin(), v.end(), [](bool i){ return i == true; });
}

std::vector<std::vector<std::string>> perfect_cliques(std::vector<std::vector<std::string>> x) {
  // In: 
  // x: a perfect sequence of sets (denoted B_i in Lauritzen)
  //
  // Out:
  // y: a perfect sequence of the cliques
  std::vector<std::vector<std::string>> y;
}

// cliques <- function(x) {
//   # x: a perfect_sequence of sets (denoted B_i in Lauritzen)
//   C_index <- c()
//   for( b in seq_along(x) ) {
//     in_others <- vapply(X = x[-b], function(z) all(x[[b]] %in% z), TRUE)
//     if( !any(in_others) ) C_index <- c(C_index, b)
//   }
//   x[C_index]
// }

// separators <- function(C) {
//   # C: Cliques (with RIP ordering)
//   # S_j := H_{j-1} \cap C_j, H_{j-1} := \cap_k C_{k-1}, k = 1, 2, ..., j-1
//   lapply( seq_along(C), function(j) {
//     if( j == 1) {
//       return(NULL)
//     } else {
//       return(intersect(C[[j]], unlist(C[1:(j-1)]) ) )
//     }
//   })
// }

// int main () {

//   // ------------------------------------------------------------------
//   // SET OF STRINGS (vectors)
//   // ------------------------------------------------------------------
//   std::vector<std::string> v1 = {"a", "c", "b", "q", "e"};
//   std::vector<std::string> v2 = {"a", "b", "c", "d", "f", "t"};

//   // ------------------------------------------------------------------
//   // SORT
//   // ------------------------------------------------------------------
//   // std::sort(std::begin(v1), std::end(v1));
//   // std::sort(std::begin(v2), std::end(v2));
  
//   // ------------------------------------------------------------------
//   // EQUAL
//   // ------------------------------------------------------------------
//   bool is_eq = set_eq(v1, v2);
//   std::cout << "\n" << is_eq << "\n";
  
//   // ------------------------------------------------------------------
//   // INTERSECTION
//   // ------------------------------------------------------------------
//   // int q = std::max(v1.size(), v2.size());
//   // std::vector<std::string> w(q);
//   auto v3 = set_intersect(v1, v2);
//   for (auto it = v3.begin(); it != v3.end(); ++it) {
//     std::cout << *it << "\n";
//   }

//   // ------------------------------------------------------------------
//   // UNION
//   // ------------------------------------------------------------------
//   std::cout << "\n\n";
//   auto v4 = set_union(v1, v2);
//   for (auto it = v4.begin(); it != v4.end(); ++it) {
//     std::cout << *it << "\n";
//   }

//   // ------------------------------------------------------------------
//   // DIFFERENCE
//   // ------------------------------------------------------------------
//   std::cout << "\n\n";
//   auto v5 = set_diff(v1, v2);
//   for (auto it = v5.begin(); it != v5.end(); ++it) {
//     std::cout << *it << "\n";
//   }

//   std::cout << "\n\n";
//   auto v6 = set_diff(v2, v3);
//   for (auto it = v6.begin(); it != v6.end(); ++it) {
//     std::cout << *it << "\n";
//   }
//   return 0;
// }
