import java.util.Iterator;

class CachingBFS {
  private static final int INFINITY = Integer.MAX_VALUE;
  private boolean[] marked;
  private int[] edgeTo;
  private int[] distTo;
  private final CachedArrays cachedArrays;

  public static class CachedArrays implements Iterable<Integer> {
    private final boolean[] marked;
    private final int[] distTo;
    private final int[] edgeTo;
    private final Queue<Integer> changed;

    public CachedArrays(int size) {
      marked = new boolean[size];
      distTo = new int[size];
      edgeTo = new int[size];
      for (int v = 0; v < size; v++)
        distTo[v] = INFINITY;
      changed = new Queue<Integer>();
    }

    @Override
    public Iterator<Integer> iterator() { return changed.iterator(); }

    public void clear() {
      int i;
      while (!changed.isEmpty()) {
        i = changed.dequeue();
        marked[i] = false;
        distTo[i] = INFINITY;
        edgeTo[i] = 0;
      }
    }

    public int size() { return marked.length; }

    public void markChanged(int index) { changed.enqueue(index); }

    public boolean[] marked() { return marked; }
    public int[] distTo() { return distTo; }
    public int[] edgeTo() { return edgeTo; }
  }


  public CachingBFS(Digraph G, int s, CachedArrays c) {
    cachedArrays = instantiate(c, G.V());
    bfs(G, s);
  }

  public CachingBFS(Digraph G, Iterable<Integer> sources, CachedArrays c) {
    cachedArrays = instantiate(c, G.V());
    bfs(G, sources);
  }

  private CachedArrays instantiate(CachedArrays c, int size) {
    CachedArrays cc;

    if (c == null) {
      cc = new CachedArrays(size);
    } else {
      c.clear();
      cc = c;
    }

    marked = cc.marked();
    distTo = cc.distTo();
    edgeTo = cc.edgeTo();

    return cc;
  }

  private void bfs(Digraph G, int s) {
    Queue<Integer> q = new Queue<Integer>();

    marked[s] = true;
    distTo[s] = 0;

    cachedArrays.markChanged(s);
    q.enqueue(s);

    while (!q.isEmpty()) {
      int v = q.dequeue();

      for (int w : G.adj(v)) {
        if (!marked[w]) {
          edgeTo[w] = v;
          distTo[w] = distTo[v] + 1;
          marked[w] = true;

          cachedArrays.markChanged(w);
          q.enqueue(w);
        }
      }
    }
  }

  private void bfs(Digraph G, Iterable<Integer> sources) {
    Queue<Integer> q = new Queue<Integer>();

    for (int s : sources) {
      marked[s] = true;
      distTo[s] = 0;
      q.enqueue(s);
      cachedArrays.markChanged(s);
    }

    while (!q.isEmpty()) {
      int v = q.dequeue();

      for (int w : G.adj(v)) {
        if (!marked[w]) {
          edgeTo[w] = v;
          distTo[w] = distTo[v] + 1;
          marked[w] = true;

          cachedArrays.markChanged(w);
          q.enqueue(w);
        }
      }
    }
  }

  public int distTo(int v) {
    return distTo[v];
  }

  public boolean hasPathTo(int v) {
    return marked[v];
  }
}