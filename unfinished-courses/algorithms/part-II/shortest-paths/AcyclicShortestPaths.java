public class AcyclicShortestPaths extends SingleSourceShortestPaths {
  public AcyclicShortestPaths(EdgeWeightedDigraph G, int source) {
    super(G, source);

    implementation(G);
  }

  @Override
  protected void afterRelaxation(int v, int w) {}

  @Override
  protected void implementation(EdgeWeightedDigraph G) {
    Topological topological = new Topological(G);

    for (int v : topological.order()) {
      for (DirectedEdge edge : G.adj(v)) {
        relax(edge);
      }
    }
  }

  public static void main(String[] arguments) {
    EdgeWeightedDigraph G = new EdgeWeightedDigraph(8);

    G.addEdge(new DirectedEdge('A', 'E', 0));
    G.addEdge(new DirectedEdge('B', 'A', 15));
    G.addEdge(new DirectedEdge('B', 'E', 19));
    G.addEdge(new DirectedEdge('B', 'F', 1));
    G.addEdge(new DirectedEdge('C', 'B', 32));
    G.addEdge(new DirectedEdge('C', 'D', 36));
    G.addEdge(new DirectedEdge('C', 'H', 50));
    G.addEdge(new DirectedEdge('D', 'H', 9));
    G.addEdge(new DirectedEdge('F', 'E', 53));
    G.addEdge(new DirectedEdge('G', 'B', 69));
    G.addEdge(new DirectedEdge('G', 'C', 27));
    G.addEdge(new DirectedEdge('G', 'F', 22));
    G.addEdge(new DirectedEdge('G', 'H', 78));

    AcyclicShortestPaths acyclic = new AcyclicShortestPaths(G, 6);

    StdOut.println(acyclic.pathTo(4));
  }
}