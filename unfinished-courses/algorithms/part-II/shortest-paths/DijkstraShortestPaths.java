public class DijkstraShortestPaths extends SingleSourceShortestPaths {
  private final IndexMinPQ<Double> queue;

  public DijkstraShortestPaths(EdgeWeightedDigraph G, int source) {
    super(G, source);

    queue = new IndexMinPQ<Double>(G.V());
    implementation(G);
  }

  @Override
  protected void afterRelaxation(int v, int w) {
    if (queue.contains(w)) {
      queue.decreaseKey(w, distTo[w]);
    } else {
      queue.insert(w, distTo[w]);
    }
  }

  @Override
  protected void implementation(EdgeWeightedDigraph G) {
    queue.insert(this.source, 0.0);

    while (!queue.isEmpty()) {
      int v = queue.delMin();

      for (DirectedEdge edge : G.adj(v)) {
        relax(edge);
      }
    }
  }

  public static void main(String[] arguments) {
    EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);

    G.addEdge(new DirectedEdge('A', 'B', 29));
    G.addEdge(new DirectedEdge('C', 'B', 84));
    G.addEdge(new DirectedEdge('C', 'F', 22));
    G.addEdge(new DirectedEdge('D', 'C', 18));
    G.addEdge(new DirectedEdge('E', 'A', 1));
    G.addEdge(new DirectedEdge('F', 'A', 29));
    G.addEdge(new DirectedEdge('F', 'B', 61));
    G.addEdge(new DirectedEdge('F', 'E', 30));
    G.addEdge(new DirectedEdge('G', 'C', 56));
    G.addEdge(new DirectedEdge('G', 'F', 85));
    G.addEdge(new DirectedEdge('G', 'H', 33));
    G.addEdge(new DirectedEdge('H', 'C', 22));
    G.addEdge(new DirectedEdge('H', 'D', 11));

    DijkstraShortestPaths dijkstra = new DijkstraShortestPaths(G, 6);

    StdOut.println(dijkstra.pathTo(4));
  }
}