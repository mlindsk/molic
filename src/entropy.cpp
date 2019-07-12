#include "entropy.h"
#include <cmath>     // For: log

std::vector<std::string> paste_rows(matrix &m, std::vector<int> d) {
  int n = m.size();
  std::vector<std::string> x(n);
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

table_of_counts count_unique_efs(std::vector<std::string> x) {
  table_of_counts tab;
  for (auto & e : x) { ++tab[e]; }
  return tab;
}

double entropy(matrix &m, std::vector<int> d) {
  double n = m.size();
  table_of_counts x = count_unique_efs(paste_rows(m, d));
  double ent = 0;
  for (auto & e : x) {
    ent += e.second / n * std::log( e.second / n );
  }
  return -ent;
}
