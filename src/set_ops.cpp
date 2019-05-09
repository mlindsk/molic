#include "molic_types.h"
// --------------------------------------------------------------------
// THESE SET OPERATIONS ARE TEMPORARY - THEY ARE SLOW BECAUSE OF "sort"
// --------------------------------------------------------------------
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
bool is_subseteq(VS const& a, VS const& b) {
  for(auto const & av:a){
    if( std::find(b.begin(),b.end(),av) == b.end() )
      return false;
  }
  return true;
}
