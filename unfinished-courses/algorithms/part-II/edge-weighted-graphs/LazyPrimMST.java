public class LazyPrimMST {
  private final boolean[] marked;

  private final Queue<Edge> spanningTree;
  private final MinPQ<Edge> queue;

  public LazyPrimMST(EdgeWeightedGraph G) {
    queue = new MinPQ<Edge>();
    spanningTree = new Queue<Edge>();
    marked = new boolean[G.V()];

    // Starting from 'C'
    visit(G, 2);

    while(!queue.isEmpty()) {
      Edge minimumEdge = queue.delMin();

      int v = minimumEdge.either(),
          w = minimumEdge.other(v);

      if (marked[v] && marked[w]) {
        continue;
      }

      spanningTree.enqueue(minimumEdge);

      if (!marked[v]) {
        visit(G, v);
      }

      if (!marked[w]) {
        visit(G, w);
      }
    }
  }

  private void visit(EdgeWeightedGraph G, int vertex) {
    marked[vertex] = true;

    for (Edge edge : G.adj(vertex)) {
      if (!marked[edge.other(vertex)]) {
        queue.insert(edge);
      }
    }
  }

  public Iterable<Edge> edges() {
    return spanningTree;
  }

  public static void main(String[] arguments) {
    EdgeWeightedGraph G = new EdgeWeightedGraph(10);

    G.addEdge(new Edge('G', 'A', 16));
    G.addEdge(new Edge('A', 'B', 13));
    G.addEdge(new Edge('A', 'F', 9));
    G.addEdge(new Edge('C', 'B', 17));
    G.addEdge(new Edge('B', 'G', 2));
    G.addEdge(new Edge('C', 'H', 12));
    G.addEdge(new Edge('C', 'G', 11));
    G.addEdge(new Edge('C', 'D', 6));
    G.addEdge(new Edge('D', 'I', 15));
    G.addEdge(new Edge('D', 'J', 8));
    G.addEdge(new Edge('D', 'E', 3));
    G.addEdge(new Edge('D', 'H', 1));
    G.addEdge(new Edge('E', 'J', 4));
    G.addEdge(new Edge('G', 'F', 7));
    G.addEdge(new Edge('H', 'G', 14));
    G.addEdge(new Edge('H', 'I', 5));
    G.addEdge(new Edge('I', 'J', 10));

    LazyPrimMST spanningTree = new LazyPrimMST(G);

    for (Edge edge : spanningTree.edges()) {
      StdOut.print((int)edge.weight() + " ");
    }
  }
}