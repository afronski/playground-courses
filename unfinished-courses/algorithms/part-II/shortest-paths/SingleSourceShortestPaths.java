public abstract class SingleSourceShortestPaths {
  protected final int source;

  protected final double[] distTo;
  protected final DirectedEdge[] edgeTo;

  protected abstract void implementation(EdgeWeightedDigraph G);
  protected abstract void afterRelaxation(int v, int w);

  protected void relax(DirectedEdge edge) {
    int v = edge.from(),
        w = edge.to();

    if (distTo[w] > distTo[v] + edge.weight()) {
      distTo[w] = distTo[v] + edge.weight();
      edgeTo[w] = edge;

      afterRelaxation(v, w);
    }
  }

  public SingleSourceShortestPaths(EdgeWeightedDigraph G, int source) {
    this.source = source;

    this.distTo = new double[G.V()];
    this.edgeTo = new DirectedEdge[G.V()];

    for (int v = 0; v < G.V(); ++v) {
      this.distTo[v] = Double.POSITIVE_INFINITY;
    }

    this.distTo[this.source] = 0.0;
  }

  public double distTo(int v) {
    return distTo[v];
  }

  public Iterable<DirectedEdge> pathTo(int v) {
    if (!hasPathTo(v)) {
      return null;
    }

    Stack<DirectedEdge> path = new Stack<DirectedEdge>();

    for (DirectedEdge edge = edgeTo[v]; edge != null; edge = edgeTo[edge.from()]) {
      path.push(edge);
    }

    return path;
  }

  public boolean hasPathTo(int v) {
    return distTo[v] < Double.POSITIVE_INFINITY;
  }
}