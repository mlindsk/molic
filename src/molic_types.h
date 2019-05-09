#include <iostream>   // For:
#include <numeric>    // For: 
#include <vector>     // For: 
#include <string>     // For:
#include <algorithm>  // For: sort, std set operations etc. 
#include <stack>      // For: dfs procedure
#include <Rcpp.h>     // For: Interface to R
// [[Rcpp::plugins(cpp11)]]

// ...............
// TYPEDEFINITIONS
// ...............
using namespace Rcpp;
using VS  = std::vector<std::string>;
using VVS = std::vector<std::vector<std::string>>;
