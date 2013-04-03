public class StronglyConnectedComponents {
  private final boolean[] marked;
  private final int[] id;
  private int count;

  // Kosaraju - Sharir algorithm.
  public StronglyConnectedComponents(Digraph G) {
    marked = new boolean[G.V()];
    id = new int[G.V()];

    DepthFirstOrder dfs = new DepthFirstOrder(G.reverse());

    for (int v : dfs.order()) {
      if (!marked[v]) {
        dfs(G, v);
        ++count;
      }
    }
  }

  public boolean connected(int v, int w) {
    return id[v] == id[w];
  }

  public int components() {
    return count;
  }

  public int id(int v) {
    return id[v];
  }

  private void dfs(Digraph G, int v) {
    marked[v] = true;
    id[v] = count;

    for(int w : G.adj(v)) {
      if (!marked[w]) {
        dfs(G, w);
      }
    }
  }
}