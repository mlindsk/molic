// https://www.geeksforgeeks.org/connected-components-in-an-undirected-graph/
// C++ program to print connected components in 
// an undirected graph 
#include <iostream> 
#include <list> 
#include <algorithm>
#include <set>
using namespace std; 
  
// Graph class represents a undirected graph 
// using adjacency list representation 
class Graph 
{ 
  int N;    // No. of vertices 
  
  // Pointer to an array containing adjacency lists 
  list<int> *adj; 
  
  // A function used by DFS 
  void DFSUtil(int v, bool visited[]); 
public: 
  Graph(int N);               // Constructor 
  void addEdge(int v, int w);
  void delNode(int v);        // Just remove entry v !!!
  void neighbors(int v);      // The v'th entry!
  void connectedComponents(int v); 
}; 
  
// Method to print connected components in an 
// undirected graph 
void Graph::connectedComponents(int v) 
{ 
    // Mark all the vertices as not visited 
    bool *visited = new bool[N]; 
    for(int v = 0; v < N; v++) 
        visited[v] = false; 

    // print all reachable vertices from v 
    DFSUtil(v, visited);
    cout << endl;
} 
  
void Graph::DFSUtil(int v, bool visited[]) 
{ 
    // Mark the current node as visited and print it 
    visited[v] = true; 
    cout << v << " "; 
  
    // Recur for all the vertices 
    // adjacent to this vertex 
    list<int>::iterator i; 
    for(i = adj[v].begin(); i != adj[v].end(); ++i) 
        if(!visited[*i]) 
            DFSUtil(*i, visited); 
} 
  
Graph::Graph(int N) 
{ 
    this->N = N; 
    adj = new list<int>[N]; 
} 
  
// method to add an undirected edge 
void Graph::addEdge(int v, int w) 
{ 
    adj[v].push_back(w); 
    adj[w].push_back(v); 
} 
  
// Drive program to test above 
int main() 
{ 
    // Create a graph given in the above diagram 
    Graph g(5); // 5 vertices numbered from 0 to 4 
    g.addEdge(1, 0); 
    g.addEdge(2, 3); 
    g.addEdge(3, 4); 
  
    cout << "Following are connected components \n"; 
    g.connectedComponents(3); 

    std::set<int> example = {1, 2, 3, 4};
    auto search = example.find(2);
    
    return 0; 
} 
