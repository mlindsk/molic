#include "efs_utils.h"
#include "outlier_utils.h"
#include <cmath>     // For: log

vec_str_pair pairwise_comb(std::vector<std::string> &x) {
  int n = x.size();
  int n_complete = n * (n - 1) / 2;
  vec_str_pair dm(n_complete, std::make_pair("", ""));
  int row = 0;
  for( int i = 0; i < n; ++i ) {
    for( int j = i+1; j < n; ++j ) {
      dm[row].first  = x[i];
      dm[row].second = x[j];
      row++;
    }
  }
  return dm;
}

RCM sub_dm( RCM & dm, std::vector<std::string> & x ) {
  RCV dm_Delta = Rcpp::colnames(dm);
  RCV y = Rcpp::wrap(x);
  RIV sub_idx = Rcpp::match(y, dm_Delta);
  int n = x.size();
  RCM dm_sub(dm.nrow(), n);
  for (int i = 0; i < n; i++) {
    dm_sub(Rcpp::_ , i ) = dm(Rcpp::_, sub_idx[i] - 1);    
  }
  return dm_sub;
}

double entropy_(RCM & dm) {
  RIV    x = n_a(dm);
  double n = dm.nrow();
  double ent;
  for (auto & e : x) {
    ent += e / n * std::log( e / n );
  }
  return -ent;
}
