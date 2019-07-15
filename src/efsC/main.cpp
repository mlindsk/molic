#include "class_decomposable_graph.h"
#include "class_clique_graph.h"
#include "class_data_matrix.h"
#include "class_efs_graph.h"
#include "unordered_set_ops.h"
#include "misc_utils.h"
#include "entropy.h"

int main() {
  std::cout << "Calling from main" << "\n\n";
  data_matrix dm;
  matrix m = {
  	      {"a", "a", "c"},
  	      {"a", "c", "t"},
  	      {"c", "a", "a"}
  };
  
  std::vector<std::string> cn({"z", "x", "y"});
  dm.mat = m;
  dm.col_names = cn;
  efs_graph eg(dm);

  eg.CG.show(false);
  std::cout << "\n";

  eg.G.show();
  std::cout << "\n";

  auto em = eg.EM;
  for (auto & e : em) {
    std::cout << e.first << " - " << e.second << "\n";
  }

  /*----------------------------------------------------------*
   *              READ IN DATA FROM tgp_dat                               
   * ---------------------------------------------------------*/
  
  return 0;
}
