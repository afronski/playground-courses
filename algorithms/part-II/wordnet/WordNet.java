import java.util.HashMap;

public class WordNet {
  private final SAP paths;
  private final HashMap<Integer, String> id2synset;
  private final HashMap<String, Bag<Integer>> noun2ids;

  public WordNet(String synsets, String hypernyms) {
    id2synset = new HashMap<Integer, String>();
    noun2ids = new HashMap<String, Bag<Integer>>();
    buildSynsets(synsets);
    paths = new SAP(buildHypernyms(hypernyms, id2synset.size()));
  }

  private void buildSynsets(String synsets) {
    In file = new In(synsets);
    String[] line;
    int id;
    Bag<Integer> bag;
    while (!file.isEmpty()) {
      line = file.readLine().split(",");
      id = Integer.parseInt(line[0]);
      id2synset.put(id, line[1]);
      for (String noun : line[1].split(" ")) {
        bag = noun2ids.get(noun);
        if (bag == null) {
          bag = new Bag<Integer>();
          bag.add(id);
          noun2ids.put(noun, bag);
        }
        else {
          bag.add(id);
        }
      }
    }
  }

  private Digraph buildHypernyms(String hypernyms, int numSynsets) {
    Digraph g = new Digraph(numSynsets);
    In file = new In(hypernyms);
    String[] line;
    int id;
    while (!file.isEmpty()) {
      line = file.readLine().split(",");
      id = Integer.parseInt(line[0]);
      for (int i = 1; i < line.length; i++)
        g.addEdge(id, Integer.parseInt(line[i]));
    }
    detectCycles(g, hypernyms);
    detectRootedness(g, hypernyms);
    return g;
  }

  private void detectCycles(Digraph g, String hypernyms) {
    DirectedCycle dc = new DirectedCycle(g);
    if (dc.hasCycle()) {
      String msg = hypernyms + " does not represent a DAG";
      throw new IllegalArgumentException(msg);
    }
  }

  private void detectRootedness(Digraph g, String hypernyms) {
    int numRoots = 0;

    for (int vertex = 0; vertex < g.V(); vertex++)
      if (!g.adj(vertex).iterator().hasNext())
        numRoots++;
    if (numRoots != 1)
      throw new IllegalArgumentException(hypernyms + " is not rooted.");
  }

  public Iterable<String> nouns() { return noun2ids.keySet(); }

  public boolean isNoun(String word) { return noun2ids.containsKey(word); }

  private void areBothNouns(String nounA, String nounB) {
    if (!isNoun(nounA) || !isNoun(nounB)) {
      String msg = "One is not in the WordNet: " + nounA + " " + nounB;
      throw new IllegalArgumentException(msg);
    }
  }

  public int distance(String nounA, String nounB) {
    areBothNouns(nounA, nounB);
    return paths.length(noun2ids.get(nounA), noun2ids.get(nounB));
  }

  public String sap(String nounA, String nounB) {
    areBothNouns(nounA, nounB);
    return id2synset.get(paths.ancestor(noun2ids.get(nounA),
                      noun2ids.get(nounB)));
  }

  public static void main(String[] args) {
    WordNet wn = new WordNet(args[0], args[1]);
    while (!StdIn.isEmpty()) {
      String v = StdIn.readString();
      String w = StdIn.readString();
      if (!wn.isNoun(v)) {
        StdOut.println(v + " not in the word net");
        continue;
      }
      if (!wn.isNoun(w)) {
        StdOut.println(w + " not in the word net");
        continue;
      }
      int distance = wn.distance(v, w);
      String ancestor = wn.sap(v, w);
      StdOut.printf("distance = %d, ancestor = %s\n", distance, ancestor);
    }
  }
}