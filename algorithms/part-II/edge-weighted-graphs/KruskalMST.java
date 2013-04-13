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
}