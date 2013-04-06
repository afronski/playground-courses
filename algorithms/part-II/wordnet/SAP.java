public class SAP {
  private final Digraph g;
  private final CachingBFS.CachedArrays vcache;
  private final CachingBFS.CachedArrays wcache;

  public SAP(Digraph G) {
    g = new Digraph(G);
    vcache = new CachingBFS.CachedArrays(g.V());
    wcache = new CachingBFS.CachedArrays(g.V());
  }

  private int min(CachingBFS pv, CachingBFS pw) {
    int min, dist;
    min = -1;
    CachingBFS.CachedArrays[] its = {vcache, wcache};
    for (Iterable<Integer> it : its) {
      for (int node : it) {
        if (pv.hasPathTo(node) && pw.hasPathTo(node)) {
          dist = pv.distTo(node) + pw.distTo(node);
          if (min < 0 || dist < min)
            min = dist;
        }
      }
    }
    return min;
  }

  private int argmin(CachingBFS pv, CachingBFS pw) {
    int min, dist, argmin;
    argmin = -1;
    min = -1;
    CachingBFS.CachedArrays[] its = {vcache, wcache};
    for (Iterable<Integer> it : its) {
      for (int node : it) {
        if (pv.hasPathTo(node) && pw.hasPathTo(node)) {
          dist = pv.distTo(node) + pw.distTo(node);
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
    return min(new CachingBFS(g, v, vcache), new CachingBFS(g, w, wcache));
  }

  public int ancestor(int v, int w) {
    CachingBFS pv = new CachingBFS(g, v, vcache);
    CachingBFS pw = new CachingBFS(g, w, wcache);
    return argmin(pv, pw);
  }

  public int length(Iterable<Integer> v, Iterable<Integer> w) {
    return min(new CachingBFS(g, v, vcache), new CachingBFS(g, w, wcache));
  }

  public int ancestor(Iterable<Integer> v, Iterable<Integer> w) {
    CachingBFS pv = new CachingBFS(g, v, vcache);
    CachingBFS pw = new CachingBFS(g, w, wcache);
    return argmin(pv, pw);
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