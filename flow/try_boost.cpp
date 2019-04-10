#include <iostream>
#include <vector>
#include <algorithm>
#include <utility>
#include <boost/config.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/properties.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/adjacency_matrix.hpp>
#include <boost/graph/connected_components.hpp>
#include <boost/property_map/property_map.hpp>
#include <boost/graph/named_function_params.hpp>

using namespace std;
using namespace boost;

struct VertexData {
  std::string v_name;
  int num;
};

struct EdgeData {
  std::string e_name;
};

void example0() {
  // A simple adjaceny list with no extra properties.
  // ------------------------------------------------
  // 1 arg: How to store vertices
  // 2 arg: How to store edges
  // 3 arg: Graph type
  // 4 arg: Vertex properties
  // 5 arg: Edge properties
  typedef boost::adjacency_list<boost::vecS, boost::vecS,
				boost::undirectedS,
				VertexData,
				EdgeData
				> MyGraphType;
  
  std::vector<std::string> x = {"x1", "x2", "x3", "x4", "x5"};
  int N = 5;
  MyGraphType G(N);
  
  for(int i = 0; i < N; i++){
    G[i].v_name = "x" + boost::lexical_cast<std::string>(i+1);
    std::cout << "vertex name " << G[i].v_name << std::endl;
    // std::cout << "name getter " << get(&VertexData::v_name, G)[i] << std::endl;
    for(int j = 0; j < N; j++) { // Edges becomes sorted!
      auto e = add_edge(i, j,G).first; 
      G[e].e_name = x[i] + "|" + x[j];
    }
  }

  // std::cout << "saved edge name "  << G[e].e_name << std::endl;
  // std::cout << "edge name getter " << get(&EdgeData::e_name, G)[e] << std::endl;

  // first is the edge. second is a bool telling you whether this is a new edge
  // or an existing one.
  // auto e = add_edge(0,1,G).first; 
  // G[e].edge_name = G[0].name + "|" + G[1].name;



  
  // add_edge(0,1,G);
  // add_edge(1,2,G);

  
  // G[0].name = "x1";
  // G[1].name = "x2";
  // G[2].name = "x3";
  
  // auto vpair = vertices(G);
  // for(auto iter=vpair.first; iter!=vpair.second; iter++) {
  //   std::cout << "vertex " << *iter << std::endl;
  // }

  // auto epair = edges(G);
  // for(auto iter=epair.first; iter!=epair.second; iter++) {
  //   std::cout << "edge " << source(*iter, G) << " - " << target(*iter, G) << std::endl;
  // }
  
}

int main() {
  example0();
  return 0;
}








  /* define the graph type
     listS: selects the STL list container to store 
     the OutEdge list
     vecS: selects the STL vector container to store 
     the vertices
     directedS: selects directed edges
  */
  // typedef adjacency_list< listS, vecS, undirectedS > make_graph;
  
  // // instantiate a digraph object with 8 vertices
  // make_graph g(8);
  
  // // add some edges
  // add_edge(0, 1, g);
  // add_edge(1, 5, g);
  // add_edge(5, 6, g);
  // add_edge(2, 3, g);
  // add_edge(2, 4, g);
  // add_edge(3, 5, g);
  // add_edge(4, 5, g);
  // add_edge(5, 7, g);


// ------------------------------------
// WORKING WITH THE BOOST GRAPH LIBRARY
// ------------------------------------

// 1) How to store the graph structure (adj lists are easiest)
//    - We can also wrap an exitsting graph structure (but tricky)

// 2) How to associate properties of vertices and edges

// 3) How get those properties in/out of different algorithms (mst, bfs, etc.)

// ------------------
// SIMPLE FIRST TAKE:
// ------------------
// int main() {

//   typedef adjacency_matrix <undirectedS> Graph;
//   Graph G(7);
//   add_edge(0, 1, G);
//   add_edge(1, 2, G);
//   add_edge(3, 4, G);
//   add_edge(5, 6, G);
//   // Just keep a vector with the column-names

//   // ---------------------------------------------------------
//   // https://www.boost.org/doc/libs/1_65_1/libs/graph/doc/connected_components.html
//   // THE CONNECTIVITY COMPONENT OF VERTEX 5:
//   // ---------------------------------------------------------
//   std::vector<int> component(num_vertices(G));
//   int num = connected_components(G, &component[0]);
//   std::vector<int>::size_type i;
//   std::vector<int> cta;
//   // cout << "Total number of components: " << num << endl;
//   for (i = 0; i != component.size(); ++i) {
//     if( component[i] == component[5] ) {
//       cta.emplace_back(i);
//       cout <<  i << endl;
//     } 
//   }
//   cout << endl;
//   // ---------------------------------------------------------

//   // ---------------------------------------------------------
//   // https://stackoverflow.com/questions/49898415/boost-library-how-to-get-neighbouring-nodes
//   // THE NEIGHBORHOOD OF VERTEX 1:
//   // ---------------------------------------------------------
//   auto neighbours = boost::adjacent_vertices(1, G);
//   for (auto vd : make_iterator_range(neighbours))
//     std::cout << "1 has adjacent vertex " << vd << "\n";  
//   // ---------------------------------------------------------


//   // ---------------------------------------------------------
//   // https://stackoverflow.com/questions/49898415/boost-library-how-to-get-neighbouring-nodes
//   // OBTAINING EDGES
//   // ---------------------------------------------------------
//   auto neighbours = boost::adjacent_vertices(1, G);
//   for (auto vd : make_iterator_range(neighbours))
//     std::cout << "1 has adjacent vertex " << vd << "\n";  
//   // ---------------------------------------------------------

  
//   return 0;
// }


