public class KruskalMST {
  private final Queue<Edge> spanningTree = new Queue<Edge>();

  public KruskalMST(EdgeWeightedGraph G) {
    MinPQ<Edge> queue = new MinPQ<Edge>();

    for (Edge e : G.edges()) {
      queue.insert(e);
    }

    UF unionFind = new UF(G.V());

    while(!queue.isEmpty() && spanningTree.size() < G.V() - 1) {
      Edge edge = queue.delMin();

      int v = edge.either(),
          w = edge.other(v);

      if (!unionFind.connected(v, w)) {
        unionFind.union(v, w);
        spanningTree.enqueue(edge);
      }
    }
  }

  public Iterable<Edge> edges() {
    return spanningTree;
  }

  public static void main(String[] arguments) {
    EdgeWeightedGraph G = new EdgeWeightedGraph(10);

    G.addEdge(new Edge('F', 'A', 16));
    G.addEdge(new Edge('B', 'A', 10));
    G.addEdge(new Edge('G', 'A', 7));
    G.addEdge(new Edge('B', 'C', 17));
    G.addEdge(new Edge('H', 'B', 14));
    G.addEdge(new Edge('G', 'B', 2));
    G.addEdge(new Edge('C', 'D', 5));
    G.addEdge(new Edge('H', 'C', 1));
    G.addEdge(new Edge('E', 'D', 13));
    G.addEdge(new Edge('D', 'J', 8));
    G.addEdge(new Edge('I', 'D', 6));
    G.addEdge(new Edge('D', 'H', 3));
    G.addEdge(new Edge('E', 'J', 12));
    G.addEdge(new Edge('F', 'G', 15));
    G.addEdge(new Edge('H', 'G', 9));
    G.addEdge(new Edge('I', 'H', 4));
    G.addEdge(new Edge('J', 'I', 11));

    KruskalMST spanningTree = new KruskalMST(G);

    for (Edge edge : spanningTree.edges()) {
      StdOut.print((int)edge.weight() + " ");
    }
  }
}