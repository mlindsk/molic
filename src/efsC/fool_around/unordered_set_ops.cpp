#include <iostream>
#include <vector>
#include <unordered_set>
#include <algorithm>

using vs  = std::vector<std::string>;
using vi  = std::vector<int>;
using vvs = std::vector<vs>;
using uss = std::unordered_set<std::string>;


uss set_intersect(uss &v1, uss &v2) {
  // Tjeck which is the smallest
  uss v;
  for (auto i = v1.begin(); i != v1.end(); i++) {
    if (v2.find(*i) != v2.end()) v.insert(*i);
  }
  return v;
}

uss set_union(uss &v1, uss &v2) {
  // Tjeck which is the smallest
  for (auto i = v2.begin(); i != v2.end(); i++) {
    if (v1.find(*i) != v1.end()) v1.insert(*i);
  }
  return v1;
}

uss set_diff(uss &v1, uss &v2) {
  uss v = v2;
  for (auto i = v.begin(); i != v.end(); i++) {
    if (v1.find(*i) != v1.end()) v.erase(*i);
  }
  return v;
}

bool set_equal(uss &v1, uss &v2) {
  if(v1.size() != v2.size()) return false;
  return std::equal(std::begin(v1), std::end(v1), std::begin(v2));
}

bool set_in(const std::string & a, const uss & b) {
  return b.find(a) != b.end();
}

bool set_issubeq(uss & a, uss & b) {
  if ( a.size() > b.size () ) return false;
  for (auto & e : a) {
    if( b.find(e) == b.end() ) {
      return false;
    }    
  }
  return true;
}



int main() {

  std::vector<std::string> vecx({"b", "c", "d", "a"});
  uss x(vecx.begin(), vecx.end());
  std::vector<std::string> vecy({"d", "a"});
  uss y(vecy.begin(), vecy.end());

  uss z = set_diff(x, y);
  for (auto & e : z) {
    std::cout << e << "\n";
  }

  std::cout << "\n";
  std::cout << set_equal(x,y) << "\n";
  std::cout << "\n";
  
  bool q = set_in("a", y);
  std::cout << q << "\n";
  bool w = set_issubeq(y, x);
  std::cout << w << "\n";

  // std::vector<int> vv({1,2,3,4,5});
  // std::vector<int>::iterator vi;

  // for( vi = vv.begin() + 2; vi != vv.end(); ++vi ) {
  //   std::cout << "hey" << "\n";
  // }

  return 0;
}


