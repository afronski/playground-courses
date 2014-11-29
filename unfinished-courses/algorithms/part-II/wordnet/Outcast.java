public class Outcast {
  private final WordNet wordnet;

  public Outcast(WordNet wordnet) {
    this.wordnet = wordnet;
  }

  public String outcast(String[] nouns) {
    int[][] distances = new int[nouns.length][nouns.length];

    for (int i = 0; i < nouns.length; ++i) {
      for (int j = i + 1; j < nouns.length; ++j) {
        distances[i][j] = wordnet.distance(nouns[i], nouns[j]);
      }
    }

    return nouns[argMaxUpperTriangular(distances)];
  }

  private int argMaxUpperTriangular(int[][] distances) {
    int sum,
        max = 0,
        argmax = 0;

    for (int i = 0; i < distances.length; i++) {
      sum = 0;

      for (int j = 0; j < distances[i].length; j++) {
        if (j < i) {
          sum += distances[j][i];
        } else {
          sum += distances[i][j];
        }
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

      for (int element = 2; element < args.length; ++element) {
          String[] nouns = In.readStrings(args[element]);
          StdOut.println(args[element] + ": " + outcast.outcast(nouns));
      }
  }
}