public class DepthFirstPaths {
  private final boolean[] marked;
  private final int[] edgeTo;
  private final int start;

  public DepthFirstPaths(Graph G, int start) {
    edgeTo = new int[G.V()];
    marked = new boolean[G.V()];

    this.start = start;

    dfs(G, start);
  }

  private void dfs(Graph G, int v) {
    marked[v] = true;

    for (int w : G.adj(v)) {
      if (!marked[w]) {
        edgeTo[w] = v;

        dfs(G, w);
      }
    }
  }

  public boolean hasPathTo(int v) {
    return marked[v];
  }

  public Iterable<Integer> pathTo(int v) {
    if (!hasPathTo(v)) {
      return null;
    }

    Stack<Integer> path = new Stack<Integer>();

    for (int x = v; x != start; x = edgeTo[x]) {
        path.push(x);
    }

    path.push(start);
    return path;
  }
}