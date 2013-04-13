public class Edge implements Comparable<Edge>{
  private final int v;
  private final int w;
  private final boolean isLetterAssigned;

  private final double weight;

  public Edge(int v, int w, double weight) {
    this.v = v;
    this.w = w;

    this.weight = weight;
    this.isLetterAssigned = false;
  }

  public Edge(char v, char w, double weight) {
    this.v = v - 'A';
    this.w = w - 'A';

    this.weight = weight;
    this.isLetterAssigned = true;
  }

  public int either() {
    return v;
  }

  public int other(int vertex) {
    if (v == vertex) {
      return w;
    } else {
      return v;
    }
  }

  public double weight() {
    return weight;
  }

  @Override
  public int compareTo(Edge that) {
    if (this.weight < that.weight) {
      return -1;
    } else if (this.weight > that.weight) {
      return 1;
    }

    return 0;
  }

  @Override
  public String toString() {
    String result = " --(%)--> ".replace("%", new Double(this.weight).toString());

    if (this.isLetterAssigned) {
      result = (char)(this.v + 'A') + result + (char)(this.w + 'A');
    } else {
      result = this.v + result + this.w;
    }

    return result;
  }

  public static void main(String[] arguments) {
    Edge e1 = new Edge('A', 'B', 0.12);
    Edge e2 = new Edge(1, 2, 0.12);

    StdOut.println(e1);
    StdOut.println(e2);
  }
}