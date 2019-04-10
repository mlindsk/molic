// https://teuder.github.io/rcpp4everyone_en/210_rcpp_functions.html
#include <iostream>
#include <RcppArmadillo.h>
// #include <boost.h>

using namespace Rcpp;
using namespace std;
// [[Rcpp::depends(RcppArmadillo)]]



  
// void efs_init_(CharacterMatrix df) { // https://teuder.github.io/rcpp4everyone_en/140_dataframe.html
//   // Input should be:
//   // - df / named CharacterMatrix
  
//   // Output should be: 
//   // - List out <- list(G = G, G_A = G_A, CG = CG, CG_A = CG_A, MSI = msi, ht = ht)

//   // What container to use for msi$S, msi$C1, msi$C2 etc?
//   // Boost library set ?
//   // http://www.cplusplus.com/reference/unordered_set/unordered_set/insert/
//   // boost::unordered_set<std::string> C1;

//   // Rcpp Lists:
//   // https://teuder.github.io/rcpp4everyone_en/150_list.html

//   // std::vector<std::string> x
//   // x = DELTA

//   // const int N = 5 //x.size();
//   // const int n_complete = N * N / 2;
  
//   // CharacterMatrix m(N,N);
//   // auto N = 8;
//   NumericMatrix::Sub sub = df( Range(0,1) , Range(2,2) );

//   // cout << "\n" << *sub(1,1) << "\n";
//   // for(int i = 0; i < N; i++){
//   //     for(int j = 0;j < N; j++) {
//   // 	cout << df(i, j) << " ";
//   //     }
//   //     cout << endl;
//   // }
  
//   // std::vector<std::pair<std::string, std::string>> p;
  
//   // Creating all pairwise combinations
//   // for (auto it = x.begin(); it != x.end(); ++it ) {
//   //   for (auto next = std::next(it); next != x.end(); ++next ) {
//   //     // MAYBE CALCULATE THE ENTROPY AT THE SAME TIME HERE
//   //     // AND CREATE THE msi LIST ON THE FLY !?
//   //     // p.emplace_back(std::make_pair(*it, *next));
//   //     // cout << "(" << *it << ", " << *next << ")" << "\n";
//   //   }
//   // }
//   // return List;
// }

// // [[Rcpp::export]]
// NumericVector timesTwo(NumericVector x) {
//   return x * 2;
// }

// // [[Rcpp::export]]
// int f() {
//   std::unordered_map< std::string, int > x = {
// 					      {"first", 1},
// 					      {"second", 2}
//   };
//   return x["second"];
// }


// // [[Rcpp::export]]
// void g(CharacterMatrix x) {
//   // Rcpp::as() expects a SEXP as input, not a Rcpp::CharacterVector.
//   std::cout << "\n" << as<std::string>(x[1,2]) + " YES !" << "\n";
//   cout << x(_, 1)[0] << " - " << x(_, 1)[1] << "\n";
//   // http://linerocks.blogspot.com/2017/12/matrix-construction-in-c-using-armadillo.html
//   // arma::mat A = arma::randu(4,4);
//   // std::cout << A[1,2];
// }

// struct efs_obj {
//   G_A;
//   CG;
//   CG_A;
//   MSI;
//   ht;
// }

// [[Rcpp::export]]
CharacterMatrix pcomb(std::vector<std::string> x) {
  auto N = x.size();
  int n_complete = N * (N - 1) / 2;
  CharacterMatrix A(n_complete, 2);
  int row = 0;
  for( int i = 0; i < N; ++i ) {
    for( int j = i+1; j < N; ++j ) {
      A(row, 0) = x[i];
      A(row, 1) = x[j];
      row += 1;
    }
  }
  return A;
}

/***R
y <- paste0("x", 1:100)
microbenchmark::microbenchmark(pcomb(y), combn(y, 2,  simplify = FALSE))
*/

// /*** R
// # y <- structure(1:2, names = c("first", "second"))
// # g(matrix(letters[1:4], 2, 2))
// n <- 100
// fact_(n)
// # y <- paste0("x", 1:n)
// # microbenchmark::microbenchmark(pcomb(y), combn(y, 2,  simplify = FALSE))
// */
