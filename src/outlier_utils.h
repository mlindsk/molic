#ifndef OUTLIER_UTILS_H
#define OUTLIER_UTILS_H

#include <Rcpp.h>
#include <vector>

using VD  = std::vector<double>;
using VS  = std::vector<std::string>;
using RL  = Rcpp::List;
using RIV = Rcpp::IntegerVector;
using RCV = Rcpp::CharacterVector;
using RCM = Rcpp::CharacterMatrix;

VS     matpr(Rcpp::CharacterMatrix A);
RIV    n_a(RCM & A);
RIV    n_b(RIV & na, RIV & b);
VD     subtract_one(VD x);
VD     Gx_(VD x);
VD     Hx_(VD x);
RCM    subM( RCM & A, RCV & x );
RL     a_marginals( RCM A, RL & am );
double TY(RCV y, RL & C_marginals, RL & S_marginals);

#endif
