public class RedBlackTree<Key extends Comparable<Key>, Value> {

  private static final boolean RED = true;
  private static final boolean BLACK = false;

  private Node root;

  private class Node {
    private Key key;
    private Value val;

    private Node left, right;

    private boolean color;
    private int N;

    public Node(Key key, Value val, boolean color, int N) {
      this.key = key;
      this.val = val;
      this.color = color;
      this.N = N;
    }
  }

  private boolean isRed(Node x) {
    if (x == null) {
      return false;
    }

    return (x.color == RED);
  }

  private int size(Node x) {
    if (x == null) {
      return 0;
    }

    return x.N;
  }

  public int size() {
    return size(root);
  }

  public boolean isEmpty() {
    return root == null;
  }

  public Value get(Key key) {
    return get(root, key);
  }

  private Value get(Node x, Key key) {
    while (x != null) {
      int cmp = key.compareTo(x.key);

      if (cmp < 0) {
        x = x.left;
      } else if (cmp > 0) {
        x = x.right;
      } else {
        return x.val;
      }
    }

    return null;
  }

  public Boolean getColor(Key key) {
    return getColor(root, key);
  }

  private Boolean getColor(Node x, Key key) {
    while (x != null) {
      int cmp = key.compareTo(x.key);

      if (cmp < 0) {
        x = x.left;
      } else if (cmp > 0) {
        x = x.right;
      } else {
        return x.color;
      }
    }

    return null;
  }

  public boolean contains(Key key) {
    return (get(key) != null);
  }

  private boolean contains(Node x, Key key) {
    return (get(x, key) != null);
  }

  public void put(Key key, Value val) {
    root = put(root, key, val);
    root.color = BLACK;

    assert check();
  }

  private Node put(Node h, Key key, Value val) {
    if (h == null) {
      return new Node(key, val, RED, 1);
    }

    int cmp = key.compareTo(h.key);

    if (cmp < 0) {
      h.left = put(h.left, key, val);
    } else if (cmp > 0) {
      h.right = put(h.right, key, val);
    } else {
      h.val = val;
    }

    if (isRed(h.right) && !isRed(h.left)) {
      h = rotateLeft(h);
    }

    if (isRed(h.left) && isRed(h.left.left)) {
      h = rotateRight(h);
    }

    if (isRed(h.left) && isRed(h.right)) {
      flipColors(h);
    }

    h.N = size(h.left) + size(h.right) + 1;

    return h;
  }

  public void deleteMin() {
    if (isEmpty()) {
      throw new RuntimeException("BST underflow");
    }

    if (!isRed(root.left) && !isRed(root.right)) {
      root.color = RED;
    }

    root = deleteMin(root);

    if (!isEmpty()) {
      root.color = BLACK;
    }

    assert check();
  }

  private Node deleteMin(Node h) {
    if (h.left == null) {
      return null;
    }

    if (!isRed(h.left) && !isRed(h.left.left)) {
      h = moveRedLeft(h);
    }

    h.left = deleteMin(h.left);

    return balance(h);
  }

  public void deleteMax() {
    if (isEmpty()) {
      throw new RuntimeException("BST underflow");
    }

    if (!isRed(root.left) && !isRed(root.right)) {
      root.color = RED;
    }

    root = deleteMax(root);

    if (!isEmpty()) {
      root.color = BLACK;
    }

    assert check();
  }

  private Node deleteMax(Node h) {
    if (isRed(h.left)) {
      h = rotateRight(h);
    }

    if (h.right == null) {
      return null;
    }

    if (!isRed(h.right) && !isRed(h.right.left)) {
      h = moveRedRight(h);
    }

    h.right = deleteMax(h.right);

    return balance(h);
  }

  public void delete(Key key) {
    if (!contains(key)) {
      System.err.println("symbol table does not contain " + key);
      return;
    }

    if (!isRed(root.left) && !isRed(root.right)) {
      root.color = RED;
    }

    root = delete(root, key);

    if (!isEmpty()) {
      root.color = BLACK;
    }

    assert check();
  }

  private Node delete(Node h, Key key) {
    assert contains(h, key);

    if (key.compareTo(h.key) < 0) {
      if (!isRed(h.left) && !isRed(h.left.left)) {
        h = moveRedLeft(h);
      }

      h.left = delete(h.left, key);
    } else {
      if (isRed(h.left)) {
        h = rotateRight(h);
      }

      if (key.compareTo(h.key) == 0 && (h.right == null)) {
        return null;
      }

      if (!isRed(h.right) && !isRed(h.right.left)) {
        h = moveRedRight(h);
      }

      if (key.compareTo(h.key) == 0) {
        h.val = get(h.right, min(h.right).key);
        h.key = min(h.right).key;
        h.right = deleteMin(h.right);
      } else {
        h.right = delete(h.right, key);
      }
    }

    return balance(h);
  }

  private Node rotateRight(Node h) {
    assert (h != null) && isRed(h.left);

    Node x = h.left;
    h.left = x.right;
    x.right = h;

    x.color = x.right.color;
    x.right.color = RED;

    x.N = h.N;
    h.N = size(h.left) + size(h.right) + 1;

    return x;
  }

  private Node rotateLeft(Node h) {
    assert (h != null) && isRed(h.right);

    Node x = h.right;
    h.right = x.left;
    x.left = h;

    x.color = x.left.color;
    x.left.color = RED;

    x.N = h.N;
    h.N = size(h.left) + size(h.right) + 1;

    return x;
  }

  private void flipColors(Node h) {
    assert (h != null) && (h.left != null) && (h.right != null);
    assert (!isRed(h) && isRed(h.left) && isRed(h.right))
        || (isRed(h) && !isRed(h.left) && !isRed(h.right));

    h.color = !h.color;
    h.left.color = !h.left.color;
    h.right.color = !h.right.color;
  }

  private Node moveRedLeft(Node h) {
    assert (h != null);
    assert isRed(h) && !isRed(h.left) && !isRed(h.left.left);

    flipColors(h);

    if (isRed(h.right.left)) {
      h.right = rotateRight(h.right);
      h = rotateLeft(h);
    }

    return h;
  }

  private Node moveRedRight(Node h) {
    assert (h != null);
    assert isRed(h) && !isRed(h.right) && !isRed(h.right.left);

    flipColors(h);

    if (isRed(h.left.left)) {
      h = rotateRight(h);
    }

    return h;
  }

  private Node balance(Node h) {
    assert (h != null);

    if (isRed(h.right)) {
      h = rotateLeft(h);
    }

    if (isRed(h.left) && isRed(h.left.left)) {
      h = rotateRight(h);
    }

    if (isRed(h.left) && isRed(h.right)) {
      flipColors(h);
    }

    h.N = size(h.left) + size(h.right) + 1;

    return h;
  }

  public int height() {
    return height(root);
  }

  private int height(Node x) {
    if (x == null) {
      return 0;
    }
    return 1 + Math.max(height(x.left), height(x.right));
  }

  public Key min() {
    if (isEmpty()) {
      return null;
    }

    return min(root).key;
  }

  private Node min(Node x) {
    assert x != null;

    if (x.left == null) {
      return x;
    }

    return min(x.left);
  }

  public Key max() {
    if (isEmpty()) {
      return null;
    }

    return max(root).key;
  }

  private Node max(Node x) {
    assert x != null;

    if (x.right == null) {
      return x;
    }

    return max(x.right);
  }

  public Key floor(Key key) {
    Node x = floor(root, key);

    if (x == null) {
      return null;
    } else {
      return x.key;
    }
  }

  private Node floor(Node x, Key key) {
    if (x == null) {
      return null;
    }

    int cmp = key.compareTo(x.key);

    if (cmp == 0) {
      return x;
    }

    if (cmp < 0) {
      return floor(x.left, key);
    }

    Node t = floor(x.right, key);

    if (t != null) {
      return t;
    } else {
      return x;
    }
  }

  public Key ceiling(Key key) {
    Node x = ceiling(root, key);

    if (x == null) {
      return null;
    } else {
      return x.key;
    }
  }

  private Node ceiling(Node x, Key key) {
    if (x == null) {
      return null;
    }

    int cmp = key.compareTo(x.key);

    if (cmp == 0) {
      return x;
    }

    if (cmp > 0) {
      return ceiling(x.right, key);
    }

    Node t = ceiling(x.left, key);

    if (t != null) {
      return t;
    } else {
      return x;
    }
  }

  public Key select(int k) {
    if (k < 0 || k >= size()) {
      return null;
    }

    Node x = select(root, k);
    return x.key;
  }

  private Node select(Node x, int k) {
    assert x != null;
    assert k >= 0 && k < size(x);

    int t = size(x.left);

    if (t > k) {
      return select(x.left, k);
    } else if (t < k) {
      return select(x.right, k - t - 1);
    } else {
      return x;
    }
  }

  public int rank(Key key) {
    return rank(key, root);
  }

  private int rank(Key key, Node x) {
    if (x == null) {
      return 0;
    }

    int cmp = key.compareTo(x.key);

    if (cmp < 0) {
      return rank(key, x.left);
    } else if (cmp > 0) {
      return 1 + size(x.left) + rank(key, x.right);
    } else {
      return size(x.left);
    }
  }

  public Iterable<Key> keys() {
    return keys(min(), max());
  }

  public Iterable<Key> keys(Key lo, Key hi) {
    Queue<Key> queue = new Queue<Key>();

    keys(root, queue, lo, hi);

    return queue;
  }

  private void keys(Node x, Queue<Key> queue, Key lo, Key hi) {
    if (x == null) {
      return;
    }

    int cmplo = lo.compareTo(x.key);
    int cmphi = hi.compareTo(x.key);

    if (cmplo < 0) {
      keys(x.left, queue, lo, hi);
    }

    if (cmplo <= 0 && cmphi >= 0) {
      queue.enqueue(x.key);
    }

    if (cmphi > 0) {
      keys(x.right, queue, lo, hi);
    }
  }

  public int size(Key lo, Key hi) {
    if (lo.compareTo(hi) > 0) {
      return 0;
    }

    if (contains(hi)) {
      return rank(hi) - rank(lo) + 1;
    } else {
      return rank(hi) - rank(lo);
    }
  }

  private boolean check() {
    if (!isBST()) {
      StdOut.println("Not in symmetric order");
    }

    if (!isSizeConsistent()) {
      StdOut.println("Subtree counts not consistent");
    }

    if (!isRankConsistent()) {
      StdOut.println("Ranks not consistent");
    }

    if (!is23()) {
      StdOut.println("Not a 2-3 tree");
    }

    if (!isBalanced()) {
      StdOut.println("Not balanced");
    }

    return isBST()
        && isSizeConsistent()
        && isRankConsistent()
        && is23()
        && isBalanced();
  }

  private boolean isBST() {
    return isBST(root, null, null);
  }

  private boolean isBST(Node x, Key min, Key max) {
    if (x == null) {
      return true;
    }

    if (min != null && x.key.compareTo(min) <= 0) {
      return false;
    }

    if (max != null && x.key.compareTo(max) >= 0) {
      return false;
    }

    return isBST(x.left, min, x.key) && isBST(x.right, x.key, max);
  }

  private boolean isSizeConsistent() {
    return isSizeConsistent(root);
  }

  private boolean isSizeConsistent(Node x) {
    if (x == null) {
      return true;
    }

    if (x.N != size(x.left) + size(x.right) + 1) {
      return false;
    }

    return isSizeConsistent(x.left) && isSizeConsistent(x.right);
  }

  private boolean isRankConsistent() {
    for (int i = 0; i < size(); i++) {
      if (i != rank(select(i))) {
        return false;
      }
    }

    for (Key key : keys()) {
      if (key.compareTo(select(rank(key))) != 0) {
        return false;
      }
    }

    return true;
  }

  private boolean is23() {
    return is23(root);
  }

  private boolean is23(Node x) {
    if (x == null) {
      return true;
    }

    if (isRed(x.right)) {
      return false;
    }

    if (x != root && isRed(x) && isRed(x.left)) {
      return false;
    }

    return is23(x.left) && is23(x.right);
  }

  private boolean isBalanced() {
    int black = 0;
    Node x = root;

    while (x != null) {
      if (!isRed(x)) {
        black++;
      }

      x = x.left;
    }

    return isBalanced(root, black);
  }

  private boolean isBalanced(Node x, int black) {
    if (x == null) {
      return black == 0;
    }

    if (!isRed(x)) {
      black--;
    }

    return isBalanced(x.left, black) && isBalanced(x.right, black);
  }

  public void levelOrderTraversal() {
    levelOrderTraversal(root);
  }

  public void levelOrderTraversal(Node node) {
    if (node != null) {

      Queue<Node> nodes = new Queue<Node>();
      nodes.enqueue(node);

      while (nodes.size() != 0) {
        Node currentNode = nodes.dequeue();
        System.out.print(currentNode.key + " ");

        if (currentNode.left != null) {
          nodes.enqueue(currentNode.left);
        }

        if (currentNode.right != null) {
          nodes.enqueue(currentNode.right);
        }
      }
    }
  }

  public static void main(String[] args) {
    RedBlackTree<Integer, Integer> st = new RedBlackTree<Integer, Integer>();

    for (int i = 0; !StdIn.isEmpty(); i++) {
      Integer key = StdIn.readInt();
      st.put(key, i);
    }

    st.put(12, 0);
    st.put(51, 0);
    st.put(83, 0);

    st.levelOrderTraversal();

    StdOut.println();
  }
}