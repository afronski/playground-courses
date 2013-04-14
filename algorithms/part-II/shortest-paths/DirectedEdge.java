public class DirectedEdge implements Comparable<DirectedEdge> {
  private final int v;
  private final int w;

  private final double weight;
  private final boolean isLetterAssigned;

  public DirectedEdge(int v, int w, double weight) {
    this.v = v;
    this.w = w;

    this.weight = weight;
    this.isLetterAssigned = false;
  }

  public DirectedEdge(char v, char w, double weight) {
    this.v = v - 'A';
    this.w = w - 'A';

    this.weight = weight;
    this.isLetterAssigned = true;
  }

  public int from() {
    return v;
  }

  public int to() {
    return w;
  }

  public double weight() {
    return weight;
  }

  @Override
  public int compareTo(DirectedEdge that) {
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
}