public class DirectedBreadthFirstPaths {
  private static final int INFINITY = Integer.MAX_VALUE;

  private final boolean[] marked;

  private final int[] edgeTo;
  private final int[] distTo;

  public DirectedBreadthFirstPaths(Digraph G, int s) {
    marked = new boolean[G.V()];
    distTo = new int[G.V()];
    edgeTo = new int[G.V()];

    bfs(G, s);
  }

  public DirectedBreadthFirstPaths(Digraph G, Iterable<Integer> sources) {
    marked = new boolean[G.V()];
    distTo = new int[G.V()];
    edgeTo = new int[G.V()];

    for (int v = 0; v < G.V(); v++) {
      distTo[v] = INFINITY;
    }

    bfs(G, sources);
  }


  private void bfs(Digraph G, int start) {
    Queue<Integer> queue = new Queue<Integer>();

    for (int v = 0; v < G.V(); v++) {
      distTo[v] = INFINITY;
    }

    distTo[start] = 0;
    marked[start] = true;

    queue.enqueue(start);

    while (!queue.isEmpty()) {
      int v = queue.dequeue();

      for (int w : G.adj(v)) {
        if (!marked[w]) {
          edgeTo[w] = v;
          distTo[w] = distTo[v] + 1;
          marked[w] = true;

          queue.enqueue(w);
        }
      }
    }
  }

  private void bfs(Digraph G, Iterable<Integer> sources) {
    Queue<Integer> queue = new Queue<Integer>();

    for (int source : sources) {
      marked[source] = true;
      distTo[source] = 0;

      queue.enqueue(source);
    }

    while (!queue.isEmpty()) {
      int v = queue.dequeue();

      for (int w : G.adj(v)) {
        if (!marked[w]) {
          edgeTo[w] = v;
          distTo[w] = distTo[v] + 1;
          marked[w] = true;

          queue.enqueue(w);
        }
      }
    }
  }

  public boolean hasPathTo(int v) {
    return marked[v];
  }

  public int distTo(int v) {
    return distTo[v];
  }

  public Iterable<Integer> pathTo(int v) {
    if (!hasPathTo(v)) {
      return null;
    }

    Stack<Integer> path = new Stack<Integer>();
    int x;

    for (x = v; distTo[x] != 0; x = edgeTo[x]) {
      path.push(x);
    }

    path.push(x);
    return path;
  }
}