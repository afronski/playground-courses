import java.util.ArrayList;

public class Digraph {
  private final int V;
  private final ArrayList<Integer>[] adjacent;

  @SuppressWarnings("unchecked")
  public Digraph(int V) {
    this.V = V;
    this.adjacent = new ArrayList[V];

    for (int v = 0; v < this.V; ++v) {
      adjacent[v] = new ArrayList<Integer>();
    }
  }

  public void addEdge(int v, int w) {
    adjacent[v].add(w);
  }

  public void addEdge(char v, char w) {
    adjacent[v - 'A'].add(w - 'A');
  }

  public Iterable<Integer> adj(int v) {
    return adjacent[v];
  }

  public Digraph reverse() {
    Digraph R = new Digraph(V);

    for (int v = 0; v < V; ++v) {
      for (int w : adj(v)) {
        R.addEdge(w, v);
      }
    }

    return R;
  }

  public int V() {
    return V;
  }
}