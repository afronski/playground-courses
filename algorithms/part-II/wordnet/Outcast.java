public class Outcast {
  private final WordNet wordnet;

  public Outcast(WordNet wordnet) { this.wordnet = wordnet; }

  public String outcast(String[] nouns) {
    int[][] distances = new int[nouns.length][nouns.length];
    for (int i = 0; i < nouns.length; i++)
      for (int j = i + 1; j < nouns.length; j++)
        distances[i][j] = wordnet.distance(nouns[i], nouns[j]);
    return nouns[argMaxUpperTriangular(distances)];
  }

  private int argMaxUpperTriangular(int[][] a) {
    int sum, max, argmax;
    max = 0;
    argmax = 0;
    for (int i = 0; i < a.length; i++) {
      sum = 0;
      for (int j = 0; j < a[i].length; j++) {
        if (j < i)
          sum += a[j][i];
        else
          sum += a[i][j];
      }
      if (i == 0 || sum > max) {
        max = sum;
        argmax = i;
      }
    }
    return argmax;
  }

  public static void main(String[] args) {
      WordNet wordnet = new WordNet(args[0], args[1]);
      Outcast outcast = new Outcast(wordnet);

      for (int t = 2; t < args.length; t++) {
          String[] nouns = In.readStrings(args[t]);
          StdOut.println(args[t] + ": " + outcast.outcast(nouns));
      }
  }
}