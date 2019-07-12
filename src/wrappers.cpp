#include "wrappers.h"

data_matrix as_data_matrix(Rcpp::CharacterMatrix &m) {
  data_matrix out;
  Rcpp::CharacterVector    cn_ = Rcpp::colnames(m);
  std::vector<std::string> cn  = Rcpp::as<std::vector<std::string>>(cn_);
  if ( cn.size() == 0 ) throw std::range_error("No column names specified");
  out.col_names = cn;
  int nrow = m.nrow();
  for (int i = 0; i < nrow; i++) {
    Rcpp::CharacterVector v = m( i, Rcpp::_);
    auto w = Rcpp::as<std::vector<std::string>>(v);
    out.mat.push_back(w);
  }
  return out;
}
