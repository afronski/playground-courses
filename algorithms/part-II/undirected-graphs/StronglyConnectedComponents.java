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

  public static void main(String[] args) {
    Digraph G = new Digraph(10);

    G.addEdge('A', 'B');
    G.addEdge('A', 'F');

    G.addEdge('B', 'C');
    G.addEdge('B', 'G');

    G.addEdge('C', 'D');

    G.addEdge('D', 'I');
    G.addEdge('D', 'J');

    G.addEdge('E', 'D');

    G.addEdge('F', 'G');

    G.addEdge('G', 'A');
    G.addEdge('G', 'C');
    G.addEdge('G', 'H');

    G.addEdge('H', 'C');
    G.addEdge('H', 'I');

    G.addEdge('I', 'C');

    G.addEdge('J', 'I');
    G.addEdge('J', 'E');

    StronglyConnectedComponents components = new StronglyConnectedComponents(G);

    for (int v = 0; v < 10; ++v) {
      StdOut.print(components.id[v] + " ");
    }
  }
}