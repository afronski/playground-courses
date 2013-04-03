public class DepthFirstOrder {
  private final boolean[] marked;
  private final Stack<Integer> reversePostOrder;

  public DepthFirstOrder(Digraph G) {
    reversePostOrder = new Stack<Integer>();
    marked = new boolean[G.V()];

    for (int v = 0; v < G.V(); ++v) {
      if (!marked[v]) {
        dfs(G, v);
      }
    }
  }

  private void dfs(Digraph G, int v) {
    marked[v] = true;

    for(int w : G.adj(v)) {
      if (!marked[w]) {
        dfs(G, w);
      }
    }

    reversePostOrder.push(v);
  }

  public Iterable<Integer> order() {
    return reversePostOrder;
  }
}