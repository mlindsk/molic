#include "efs_utils.h"

vec_str_pair pairwise_comb(std::vector<std::string> &x) {
  int n = x.size();
  int n_complete = n * (n - 1) / 2;
  vec_str_pair A(n_complete, std::make_pair("", ""));
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
