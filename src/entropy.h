#ifndef ENTROPY_H
#define ENTROPY_H

#include <vector>
#include <string>
#include <unordered_map>

using table_of_counts = std::unordered_map<std::string, int>;
using matrix = std::vector<std::vector<std::string>>;

table_of_counts count_unique(std::vector<std::string> &x);
std::vector<std::string> paste_rows(matrix &m, std::vector<int> d);
double entropy(matrix &m, std::vector<int> d);

#endif
