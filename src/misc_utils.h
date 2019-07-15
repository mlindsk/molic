#ifndef MISC_UTILS_H
#define MISC_UTILS_H

#include <Rcpp.h>
#include <vector>
#include <string>

using VS  = std::vector<std::string>;

/**
 * This is the old dfs version that is used in efs_utils/efs_step.R 
 */
VS dfs(Rcpp::List adjList, std::string root);

#endif
