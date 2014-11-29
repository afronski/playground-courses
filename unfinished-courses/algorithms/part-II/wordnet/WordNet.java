import java.util.HashMap;

public class WordNet {
  private final SAP paths;

  private final HashMap<Integer, String> synsetsHashMap;
  private final HashMap<String, Bag<Integer>> nounIdsHashMap;

  public WordNet(String synsets, String hypernyms) {
    synsetsHashMap = new HashMap<Integer, String>();
    nounIdsHashMap = new HashMap<String, Bag<Integer>>();

    buildSynsets(synsets);

    paths = new SAP(buildHypernyms(hypernyms, synsetsHashMap.size()));
  }

  private void buildSynsets(String synsets) {
    In file = new In(synsets);

    while (!file.isEmpty()) {
      String[] line = file.readLine().split(",");
      int id = Integer.parseInt(line[0]);

      synsetsHashMap.put(id, line[1]);

      for (String noun : line[1].split(" ")) {
        Bag<Integer> bag = nounIdsHashMap.get(noun);

        if (bag == null) {
          bag = new Bag<Integer>();
          bag.add(id);

          nounIdsHashMap.put(noun, bag);
        } else {
          bag.add(id);
        }
      }
    }
  }

  private Digraph buildHypernyms(String hypernyms, int numSynsets) {
    Digraph digraph = new Digraph(numSynsets);
    In file = new In(hypernyms);

    while (!file.isEmpty()) {
      String[] line = file.readLine().split(",");
      int id = Integer.parseInt(line[0]);

      for (int i = 1; i < line.length; ++i) {
        digraph.addEdge(id, Integer.parseInt(line[i]));
      }
    }

    detectCycles(digraph, hypernyms);
    detectRootedness(digraph, hypernyms);

    return digraph;
  }

  private void detectCycles(Digraph digraph, String hypernyms) {
    DirectedCycle directedCycle = new DirectedCycle(digraph);

    if (directedCycle.hasCycle()) {
      String msg = hypernyms + " does not represent a DAG";

      throw new IllegalArgumentException(msg);
    }
  }

  private void detectRootedness(Digraph digraph, String hypernyms) {
    int numRoots = 0;

    for (int vertex = 0; vertex < digraph.V(); ++vertex) {
      if (!digraph.adj(vertex).iterator().hasNext()) {
        numRoots++;
      }
    }

    if (numRoots != 1) {
      throw new IllegalArgumentException(hypernyms + " is not rooted.");
    }
  }

  public Iterable<String> nouns() {
    return nounIdsHashMap.keySet();
  }

  public boolean isNoun(String word) {
    return nounIdsHashMap.containsKey(word);
  }

  private void areBothNouns(String nounA, String nounB) {
    if (!isNoun(nounA) || !isNoun(nounB)) {
      String msg = "One is not in the WordNet: " + nounA + " " + nounB;

      throw new IllegalArgumentException(msg);
    }
  }

  public int distance(String nounA, String nounB) {
    areBothNouns(nounA, nounB);

    return paths.length(nounIdsHashMap.get(nounA),
                        nounIdsHashMap.get(nounB));
  }

  public String sap(String nounA, String nounB) {
    areBothNouns(nounA, nounB);

    return synsetsHashMap.get(paths.ancestor(nounIdsHashMap.get(nounA),
                                             nounIdsHashMap.get(nounB)));
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