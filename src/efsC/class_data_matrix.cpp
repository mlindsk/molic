#include "class_data_matrix.h"

void data_matrix::show(int nrow, int ncol) {
  for (int i = 0; i < ncol; i++) {
    std::cout << col_names[i] << " ";
  }
  std::cout << "\n";
  for (int i = 0; i < nrow; i++) {
    for (int j = 0; j < ncol; j++) {
      std::cout << mat[i][j] << " ";
    }
    std::cout << "\n";
  }
}
