public class SAP {
  private final Digraph digraph;

  private final CachingBFS.CachedArrays vCache;
  private final CachingBFS.CachedArrays wCache;

  public SAP(Digraph G) {
    digraph = new Digraph(G);

    vCache = new CachingBFS.CachedArrays(digraph.V());
    wCache = new CachingBFS.CachedArrays(digraph.V());
  }

  private int min(CachingBFS lengthsForV, CachingBFS lengthsForW) {
    int min = -1,
        dist;

    CachingBFS.CachedArrays[] caches = { vCache, wCache };

    for (Iterable<Integer> nodes : caches) {
      for (int node : nodes) {
        if (lengthsForV.hasPathTo(node) && lengthsForW.hasPathTo(node)) {
          dist = lengthsForV.distTo(node) + lengthsForW.distTo(node);

          if (min < 0 || dist < min) {
            min = dist;
          }
        }
      }
    }

    return min;
  }

  private int argmin(CachingBFS lengthsForV, CachingBFS lengthsForW) {
    int min = -1,
        argmin = -1,
        dist;

    CachingBFS.CachedArrays[] caches = { vCache, wCache };

    for (Iterable<Integer> nodes : caches) {
      for (int node : nodes) {
        if (lengthsForV.hasPathTo(node) && lengthsForW.hasPathTo(node)) {
          dist = lengthsForV.distTo(node) + lengthsForW.distTo(node);

          if (min < 0 || dist < min) {
            min = dist;
            argmin = node;
          }
        }
      }
    }

    return argmin;
  }

  public int length(int v, int w) {
    return min(new CachingBFS(digraph, v, vCache),
               new CachingBFS(digraph, w, wCache));
  }

  public int ancestor(int v, int w) {
    return argmin(new CachingBFS(digraph, v, vCache),
                  new CachingBFS(digraph, w, wCache));
  }

  public int length(Iterable<Integer> v, Iterable<Integer> w) {
    return min(new CachingBFS(digraph, v, vCache),
               new CachingBFS(digraph, w, wCache));
  }

  public int ancestor(Iterable<Integer> v, Iterable<Integer> w) {
    return argmin(new CachingBFS(digraph, v, vCache),
                  new CachingBFS(digraph, w, wCache));
  }

  public static void main(String[] args) {
    In in = new In(args[0]);

    Digraph G = new Digraph(in);
    SAP sap = new SAP(G);

    while (!StdIn.isEmpty()) {
      int v = StdIn.readInt();
      int w = StdIn.readInt();

      int length   = sap.length(v, w);
      int ancestor = sap.ancestor(v, w);

      StdOut.printf("length = %d, ancestor = %d\n", length, ancestor);
    }
  }
}