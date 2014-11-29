import java.util.ArrayList;

public class EdgeWeightedDigraph {
  private final int V;
  private int E;

  private final ArrayList<DirectedEdge>[] adjacent;

  @SuppressWarnings("unchecked")
  public EdgeWeightedDigraph(int V) {
    this.V = V;
    this.E = 0;
    this.adjacent = new ArrayList[V];

    for (int v = 0; v < this.V; ++v) {
      adjacent[v] = new ArrayList<DirectedEdge>();
    }
  }

  public void addEdge(DirectedEdge edge) {
    int v = edge.from();

    adjacent[v].add(edge);

    ++E;
  }

  public Iterable<DirectedEdge> adj(int v) {
    return adjacent[v];
  }

  public Iterable<DirectedEdge> edges() {
    ArrayList<DirectedEdge> list = new ArrayList<DirectedEdge>();

    for (int vertex = 0; vertex < V; ++vertex) {
      int selfLoops = 0;

      for (DirectedEdge edge : adj(vertex)) {
        if (edge.from() > vertex) {
            list.add(edge);
        } else if (edge.to() == vertex) {
          if (selfLoops % 2 == 0) {
            list.add(edge);
          }

          selfLoops++;
        }
      }
    }

    return list;
  }

  public int V() {
    return V;
  }

  public int E() {
    return E;
  }
}