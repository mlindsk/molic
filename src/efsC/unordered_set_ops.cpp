#include <iostream>
#include <vector>
#include <algorithm>
#include <unordered_set>

using set = std::unordered_set<std::string>;

set set_intersect(set &v1, set &v2) {
  // Tjeck which is the smallest
  set v;
  for (auto i = v1.begin(); i != v1.end(); i++) {
    if (v2.find(*i) != v2.end()) v.insert(*i);
  }
  return v;
}

set set_union(set &v1, set &v2) {
  // Tjeck which is the smallest
  for (auto i = v2.begin(); i != v2.end(); i++) {
    if (v1.find(*i) != v1.end()) v1.insert(*i);
  }
  return v1;
}

set set_diff(set &v1, set &v2) {
  set v = v2;
  for (auto i = v.begin(); i != v.end(); i++) {
    if (v1.find(*i) != v1.end()) v.erase(*i);
  }
  return v;
}

bool set_equal(set &v1, set &v2) {
  if(v1.size() != v2.size()) return false;
  return std::equal(std::begin(v1), std::end(v1), std::begin(v2));
}

bool set_in(const std::string & a, const set & b) {
  return b.find(a) != b.end();
}

bool set_issubeq(set & a, set & b) {
  if ( a.size() > b.size () ) return false;
  for (auto & e : a) {
    if( b.find(e) == b.end() ) {
      return false;
    }    
  }
  return true;
}



// int main() {

//   std::vector<std::string> vecx({"b", "c", "d", "a"});
//   set x(vecx.begin(), vecx.end());
//   std::vector<std::string> vecy({"d", "a"});
//   set y(vecy.begin(), vecy.end());

//   set z = set_diff(x, y);
//   for (auto & e : z) {
//     std::cout << e << "\n";
//   }

//   std::cout << "\n";
//   std::cout << set_equal(x,y) << "\n";
//   std::cout << "\n";
  
//   bool q = set_in("a", y);
//   std::cout << q << "\n";
//   bool w = set_issubeq(y, x);
//   std::cout << w << "\n";

//   // std::vector<int> vv({1,2,3,4,5});
//   // std::vector<int>::iterator vi;

//   // for( vi = vv.begin() + 2; vi != vv.end(); ++vi ) {
//   //   std::cout << "hey" << "\n";
//   // }

//   return 0;
// }


