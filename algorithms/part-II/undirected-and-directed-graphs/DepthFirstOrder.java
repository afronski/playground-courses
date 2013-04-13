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

  public static void main(String[] args) {
    Digraph G = new Digraph(8);

    G.addEdge('A', 'E');

    G.addEdge('B', 'E');
    G.addEdge('B', 'A');
    G.addEdge('B', 'C');

    G.addEdge('C', 'D');
    G.addEdge('C', 'G');

    G.addEdge('F', 'B');
    G.addEdge('F', 'E');
    G.addEdge('F', 'C');
    G.addEdge('F', 'G');

    G.addEdge('G', 'D');
    G.addEdge('G', 'H');

    G.addEdge('H', 'D');

    DepthFirstOrder dfo = new DepthFirstOrder(G);

    for (int v : dfo.order()) {
      StdOut.print((char)(v + 'A') + " ");
    }
  }
}