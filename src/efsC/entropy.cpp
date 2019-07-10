#include "entropy.h"

/*-------------------------------------------------------------*
 * In: dm - data matrix with each cell being a string of one char
 *     d  - column indices for which to paste rows
 * Out: vector of pasted rows
 *------------------------------------------------------------*/
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

/*----------------------------------------------------------*
 * In: x - Vector of strings
 * Out: Counts of all unique elements in x
 * ---------------------------------------------------------*/
table count_unique(std::vector<std::string> x) {
  table tab;
  for (auto & e : x) { ++tab[e]; }
  return tab;
}

/*----------------------------------------------------------*
 * In: dm - data matrix
 *     d  - column indices for which to paste rows
 * Out: The joint entropy over variables defined by comlumns d
 *---------------------------------------------------------*/
double entropy(matrix &m, std::vector<int> d) {
  double n = m.size();
  table x = count_unique(paste_rows(m, d));
  double ent = 0;
  for (auto & e : x) {
    ent += e.second / n * std::log( e.second / n );
  }
  return -ent;
}
