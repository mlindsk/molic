#ifndef ENTROPY_H
#define ENTROPY_H

using matrix = std::vector<std::vector<std::string>>;
using table  = std::unordered_map<std::string, int>;

table count_unique(std::vector<std::string> &x);
std::vector<std::string> paste_rows(matrix &m, std::vector<int> d);
double entropy(matrix &m, std::vector<int> d);

#endif
