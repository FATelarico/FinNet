#include <Rcpp.h>
#include <vector>
using namespace std;
using namespace Rcpp;

class GFG {
public:
  // dfs Function to reach destination
  bool dfs(int curr, int des, vector<vector<int> >& adj,
           vector<int>& vis)
  {
    // If curr node is destination return true
    if (curr == des) {
      return true;
    }
    vis[curr] = 1;
    for (auto x : adj[curr]) {
      if (!vis[x]) {
        if (dfs(x, des, adj, vis)) {
          return true;
        }
      }
    }
    return false;
  }

  // To tell whether there is path from source to
  // destination
  bool isPath(int src, int des, vector<vector<int> >& adj)
  {
    vector<int> vis(adj.size() + 1, 0);
    return dfs(src, des, adj, vis);
  }

  // Function to return all the strongly connected
  // component of a graph.
  vector<vector<int> > findSCC(int n,
                               vector<vector<int> >& a)
  {
    // Stores all the strongly connected components.
    vector<vector<int> > ans;

    // Stores whether a vertex is a part of any Strongly
    // Connected Component
    vector<int> is_scc(n + 1, 0);

    vector<vector<int> > adj(n + 1);

    for (size_t i = 0; i < a.size(); i++) {
      adj[a[i][0]].push_back(a[i][1]);
    }

    // Traversing all the vertices
    for (int i = 1; i <= n; i++) {

      if (!is_scc[i]) {

        // If a vertex i is not a part of any SCC
        // insert it into a new SCC list and check
        // for other vertices whether they can be
        // thr part of thidl ist.
        vector<int> scc;
        scc.push_back(i);

        for (int j = i + 1; j <= n; j++) {

          // If there is a path from vertex i to
          // vertex j and vice versa put vertex j
          // into the current SCC list.
          if (!is_scc[j] && isPath(i, j, adj)
                && isPath(j, i, adj)) {
            is_scc[j] = 1;
            scc.push_back(j);
          }
        }

        // Insert the SCC containing vertex i into
        // the final list.
        ans.push_back(scc);
      }
    }
    return ans;
  }
};

// [[Rcpp::export]]
std::vector<std::vector<int>> findSCC(int n, std::vector<std::vector<int>> edges) {
  GFG obj;
  return obj.findSCC(n, edges);
}
