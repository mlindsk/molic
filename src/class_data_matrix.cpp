#include <Rcpp.h>
#include "class_data_matrix.h"

void data_matrix::show(int nrow, int ncol) {
  for (int i = 0; i < ncol; i++) {
    Rcpp::Rcout << col_names[i] << " ";
  }
  Rcpp::Rcout << "\n";
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      Rcpp::Rcout << mat[i][j] << " ";
    }
    Rcpp::Rcout << "\n";
  }
}
