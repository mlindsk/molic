#include "class_data_matrix.h"
#include "wrappers.h"
#include "Rcpp.h"
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
void mainC(Rcpp::CharacterMatrix &m, Rcpp::CharacterVector &nodes) {
  Rcpp::Rcout << "\n\n  ----- Calling from mainC.cpp -------" << "\n\n";
  data_matrix dm = as_data_matrix(m);
  dm.show(3, 3);
}
