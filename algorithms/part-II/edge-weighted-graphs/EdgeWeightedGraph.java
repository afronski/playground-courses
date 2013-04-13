import java.util.ArrayList;

public class EdgeWeightedGraph {
  private final int V;
  private int E;
  private final ArrayList<Edge>[] adjacent;

  @SuppressWarnings("unchecked")
  public EdgeWeightedGraph(int V) {
    this.V = V;
    this.E = 0;
    this.adjacent = new ArrayList[V];

    for (int v = 0; v < this.V; ++v) {
      adjacent[v] = new ArrayList<Edge>();
    }
  }

  public void addEdge(Edge edge) {
    int v = edge.either(),
        w = edge.other(v);

    adjacent[v].add(edge);
    adjacent[w].add(edge);

    ++E;
  }

  public Iterable<Edge> adj(int v) {
    return adjacent[v];
  }

  public Iterable<Edge> edges() {
    ArrayList<Edge> list = new ArrayList<Edge>();

    for (int vertex = 0; vertex < V; ++vertex) {
      int selfLoops = 0;

      for (Edge edge : adj(vertex)) {
        if (edge.other(vertex) > vertex) {
            list.add(edge);
        } else if (edge.other(vertex) == vertex) {
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