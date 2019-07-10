#ifndef DATA_MATRIX_H
#define DATA_MATRIX_H

#include <iostream>
#include <vector>
#include <string>

using matrix = std::vector<std::vector<std::string>>;

struct data_matrix {
  matrix mat;
  std::vector<std::string> col_names;
  void show(int nrow, int ncol);
};

#endif
