public class BellmanFordShortestPaths extends SingleSourceShortestPaths {
  private final boolean[] onQueue;
  private final Queue<Integer> queue;

  public BellmanFordShortestPaths(EdgeWeightedDigraph G, int source) {
    super(G, source);

    onQueue = new boolean[G.V()];
    queue = new Queue<Integer>();

    queue.enqueue(this.source);
    onQueue[this.source] = true;

    implementation(G);
  }

  @Override
  protected void implementation(EdgeWeightedDigraph G) {
    while (!queue.isEmpty()) {
      int v = queue.dequeue();

      onQueue[v] = false;

      for (DirectedEdge edge : G.adj(v)) {
        relax(edge);
      }
    }
  }

  @Override
  protected void afterRelaxation(int v, int w) {
    if (!onQueue[w]) {
      queue.enqueue(w);
      onQueue[w] = true;
    }
  }

  public static void main(String[] arguments) {
    EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);

    G.addEdge(new DirectedEdge('A', 'F', 30));
    G.addEdge(new DirectedEdge('A', 'E', 34));
    G.addEdge(new DirectedEdge('B', 'A', 1));
    G.addEdge(new DirectedEdge('B', 'C', 62));
    G.addEdge(new DirectedEdge('C', 'G', 2));
    G.addEdge(new DirectedEdge('D', 'C', 31));
    G.addEdge(new DirectedEdge('F', 'B', 5));
    G.addEdge(new DirectedEdge('F', 'E', 5));
    G.addEdge(new DirectedEdge('G', 'D', 33));
    G.addEdge(new DirectedEdge('G', 'F', 50));
    G.addEdge(new DirectedEdge('G', 'B', 9));
    G.addEdge(new DirectedEdge('H', 'G', 9));
    G.addEdge(new DirectedEdge('H', 'D', 47));

    BellmanFordShortestPaths paths = new BellmanFordShortestPaths(G, 7);

    StdOut.println(paths.pathTo(4));
  }
}