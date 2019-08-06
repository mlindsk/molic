#include "set_ops.h"
// #include <algorithm>  // For: set operations/algs
// #include <numeric>
/*----------------------------------------------------------*
 *                  FOR EFS C++ VERSION                                   
 * ---------------------------------------------------------*/

// set set_intersect(set &v1, set &v2) {
//   // Tjeck which is the smallest
//   set v;
//   for (auto i = v1.begin(); i != v1.end(); i++) {
//     if (v2.find(*i) != v2.end()) v.insert(*i);
//   }
//   return v;
// }

// set set_union(set &v1, set &v2) {
//   // Tjeck which is the smallest
//   for (auto i = v2.begin(); i != v2.end(); i++) {
//     if (v1.find(*i) != v1.end()) v1.insert(*i);
//   }
//   return v1;
// }

// set set_diff(set &v1, set &v2) {
//   set v = v2;
//   for (auto i = v.begin(); i != v.end(); i++) {
//     if (v1.find(*i) != v1.end()) v.erase(*i);
//   }
//   return v;
// }

// bool set_equal(set &v1, set &v2) {
//   if(v1.size() != v2.size()) return false;
//   return std::equal(std::begin(v1), std::end(v1), std::begin(v2));
// }

// bool set_in(const std::string & a, const set & b) {
//   return b.find(a) != b.end();
// }

// bool set_issubeq(set & a, set & b) {
//   if ( a.size() > b.size () ) return false;
//   for (auto & e : a) {
//     if( b.find(e) == b.end() ) {
//       return false;
//     }    
//   }
//   return true;
// }

/*----------------------------------------------------------*
 *                     FOR OUTLIER UTILS                                   
 * ---------------------------------------------------------*/

// [[Rcpp::export]]
VS set_intersect(VS &v1, VS &v2) {
  VS v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_intersection(v1.begin(),v1.end(),
			v2.begin(),v2.end(),
			back_inserter(v));
  return v;
}

// [[Rcpp::export]]
VS set_union(VS &v1, VS &v2) {
  VS v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_union(v1.begin(),v1.end(),
		 v2.begin(),v2.end(),
		 back_inserter(v));
  return v;
}

// [[Rcpp::export]]
VS set_diff(VS &v1, VS &v2) {
  VS v;
  std::sort(v1.begin(), v1.end());
  std::sort(v2.begin(), v2.end());
  std::set_difference(v1.begin(),v1.end(),
		      v2.begin(),v2.end(),
		      back_inserter(v));
  return v;
}

// [[Rcpp::export]]
bool set_eq(VS &v1, VS &v2) {
  if(v1.size() != v2.size()) return false;
  std::sort(std::begin(v1), std::end(v1));
  std::sort(std::begin(v2), std::end(v2));
  return std::equal(std::begin(v1), std::end(v1), std::begin(v2));
}

// [[Rcpp::export]]
bool set_in(std::string & a, VS & b) {
  std::vector<std::string>::iterator it = std::find(b.begin(), b.end(), a);
  return it != b.end();
}

// [[Rcpp::export]]
bool set_issubeq(VS &a, VS &b) {
  for(auto const & av : a){
    if( std::find(b.begin(), b.end(), av) == b.end() )
      return false;
  }
  return true;
}

// [[Rcpp::export]]
bool set_any(std::vector<bool> &v) {
  return std::any_of(v.begin(), v.end(), [](bool i){ return i == true; });
}
