#include "class_efs_graph.h"
#include <vector>
#include <string>
#include <Rcpp.h>
// [[Rcpp::plugins(cpp11)]]

// http://dirk.eddelbuettel.com/code/rinside.html
// https://stackoverflow.com/questions/23304012/using-rcpp-headers-in-c
// /home/mads/R/x86_64-pc-linux-gnu-library/3.4/Rcpp/include/Rcpp.h
// #include <Rmath.h>

data_matrix as_data_matrix(Rcpp::CharacterMatrix &m) {
  data_matrix out; // MAKE A CONSTRUCTOR!!!!!!!!!!!!!
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

// [[Rcpp::export]]
void showme(Rcpp::CharacterMatrix &m) {
  data_matrix A = as_data_matrix(m);
  int r = A.mat.size();
  int c = A.mat[0].size();
  A.show(r, c);
}

// [[Rcpp::export]]
void mainR(Rcpp::CharacterMatrix &m) {
  std::cout << "Calling from mainR" << "\n\n";
  data_matrix dm = as_data_matrix(m);
  dm.show(30, 10);
  // efs_graph eg(dm);

  // eg.CG.show(false);
  // std::cout << "\n";

  // eg.G.show();
  // std::cout << "\n";

  // auto em = eg.EM;
  // for (auto & e : em) {
  //   std::cout << e.first << " - " << e.second << "\n";
  // }
}
