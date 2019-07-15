#include "class_efs_graph.h"
#include "Rcpp.h"
// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
void mainC(Rcpp::CharacterMatrix &dm) {
  Rcpp::Rcout << "\n\n  ----- Calling from mainC.cpp -------" << "\n\n";
  efs_graph G(dm);
}
