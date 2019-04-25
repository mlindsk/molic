// https://codereview.stackexchange.com/questions/175104/adjacency-list-graph-representation
// unordered_map.find(key);

// efs_init;

class efs_graph {
public:
  //=================
  // MEMBER VARIABLES
  //=================
  // Number of vertices
  int N;
  std::unordered_map<> G; // adjacency list
  // MSI
  // ht
  // CG
  // CG_A
  
  //============
  // CONSTRUCTOR
  //============
  // efs_init
  efs_graph(int N);

  //=================
  // MEMBER FUNCTIONS
  //=================
  void add_edge(int v, int w);
  void neighbors(int v); // The v'th entry!
};

// this -> pointer!
