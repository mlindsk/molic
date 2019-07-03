#include <Rcpp.h>
#include <unordered_map>
#include <iostream>
#include <vector>
#include <string>
#include <cmath>
// [[Rcpp::plugins(cpp11)]]

using vs  = std::vector<std::string>;
using vi  = std::vector<int>;
using vvs = std::vector<vs>;


/*-------------------------------------------------------------*
 * In: m - data matrix with each cell being a string of one char
 *     d - column indices for which to paste rows
 * Out: vector of pasted rows
 *------------------------------------------------------------*/
vs paste_rows(const vvs &m, const vi d) {
  int n = m.size();
  vs  x(n);
  for( int i = 0; i < n; i++ ) {
    auto row = m[i];
    std::string s;
    for (auto & e : d) {
      s += row[e];
    }
    x[i] = s;
  }
  return x;
}

/*----------------------------------------------------------*
 * In: x - Vector of strings
 * Out: Counts of all unique elements in x
 * ---------------------------------------------------------*/
std::unordered_map<std::string, int> count_unique(const vs  x) {
  std::unordered_map<std::string, int> tab;
  for (auto & e : x) { ++tab[e]; }
  return tab;
}

/*----------------------------------------------------------*
 * In: m - data matrix
 *     d - column indices for which to paste rows
 * Out: The joint entropy over variables defined by comlumns d
 *---------------------------------------------------------*/
double entropy(const vvs &m, const vi d) {
  auto x = count_unique(paste_rows(m, d));
  const double n = m.size();
  double ent = 0;
  for (auto & e : x) {
    ent += e.second / n * std::log( e.second / n );
  }
  return -ent;
}

//int main() {
  // vs x = {"a", "a", "b", "c"};
  // vvs m = { {"a", "b", "t"}, {"a", "c", "q"}, {"b", "b", "z"}  };
  // auto a = count_unique(x);
  // // std::cout << a["b"] << "\n";
  // vi d_ = {0, 2};
  // vs M = paste_rows(m, d_);
  // // std::cout << M[2] << "\n";
  // auto W = count_unique(paste_rows(m, d_));
  // for (auto & e : W) {
  //   std::cout << e.first << "\t";
  //   std::cout << e.second << "\n";
  // }
  // std::cout << entropy(m, d_) << "\n";
  
//  return 0;
//}
