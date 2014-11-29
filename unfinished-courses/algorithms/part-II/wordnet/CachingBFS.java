import java.util.Iterator;

class CachingBFS {
  private static final int INFINITY = Integer.MAX_VALUE;

  private boolean[] isMarked;
  private int[] edgeTo;
  private int[] distanceTo;

  private final CachedArrays cachedArrays;

  public static class CachedArrays implements Iterable<Integer> {
    private final boolean[] isMarked;
    private final int[] distanceTo;
    private final int[] edgeTo;

    private final Queue<Integer> changed;

    public CachedArrays(int size) {
      isMarked = new boolean[size];
      distanceTo = new int[size];
      edgeTo = new int[size];

      for (int v = 0; v < size; v++) {
        distanceTo[v] = INFINITY;
      }

      changed = new Queue<Integer>();
    }

    @Override
    public Iterator<Integer> iterator() {
      return changed.iterator();
    }

    public void clear() {
      while (!changed.isEmpty()) {
        int v = changed.dequeue();

        isMarked[v] = false;
        distanceTo[v] = INFINITY;
        edgeTo[v] = 0;
      }
    }

    public int size() {
      return isMarked.length;
    }

    public void markChanged(int index) {
      changed.enqueue(index);
    }

    public boolean[] marked() {
      return isMarked;
    }

    public int[] distTo() {
      return distanceTo;
    }

    public int[] edgeTo() {
      return edgeTo;
    }
  }

  public CachingBFS(Digraph G, int start, CachedArrays cache) {
    cachedArrays = instantiate(cache, G.V());

    bfs(G, start);
  }

  public CachingBFS(Digraph G, Iterable<Integer> sources, CachedArrays cache) {
    cachedArrays = instantiate(cache, G.V());

    bfs(G, sources);
  }

  private CachedArrays instantiate(CachedArrays cache, int size) {
    CachedArrays created;

    if (cache == null) {
      created = new CachedArrays(size);
    } else {
      cache.clear();
      created = cache;
    }

    isMarked = created.marked();
    distanceTo = created.distTo();
    edgeTo = created.edgeTo();

    return created;
  }

  private void bfs(Digraph G, int start) {
    Queue<Integer> queue = new Queue<Integer>();

    isMarked[start] = true;
    distanceTo[start] = 0;

    cachedArrays.markChanged(start);
    queue.enqueue(start);

    while (!queue.isEmpty()) {
      int v = queue.dequeue();

      for (int w : G.adj(v)) {
        if (!isMarked[w]) {
          edgeTo[w] = v;
          distanceTo[w] = distanceTo[v] + 1;
          isMarked[w] = true;

          cachedArrays.markChanged(w);
          queue.enqueue(w);
        }
      }
    }
  }

  private void bfs(Digraph G, Iterable<Integer> sources) {
    Queue<Integer> queue = new Queue<Integer>();

    for (int source : sources) {
      isMarked[source] = true;
      distanceTo[source] = 0;

      queue.enqueue(source);
      cachedArrays.markChanged(source);
    }

    while (!queue.isEmpty()) {
      int v = queue.dequeue();

      for (int w : G.adj(v)) {
        if (!isMarked[w]) {
          edgeTo[w] = v;
          distanceTo[w] = distanceTo[v] + 1;
          isMarked[w] = true;

          cachedArrays.markChanged(w);
          queue.enqueue(w);
        }
      }
    }
  }

  public int distTo(int v) {
    return distanceTo[v];
  }

  public boolean hasPathTo(int v) {
    return isMarked[v];
  }
}