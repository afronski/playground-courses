public class BST<Key extends Comparable<Key>, Value> {
    private Node root;

    private class Node {
        public Key key;
        public Value val;
        public Node left, right;

        public Node(Key key, Value val) {
            this.key = key;
            this.val = val;
        }
    }

    public Value get(Key key)
    {
        Node x = root;

        while (x != null)
        {
            StdOut.print(x.key + " ");
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

    public void put(Key key, Value val) {
        root = put(root, key, val);
    }

    private Node put(Node x, Key key, Value val)
    {
        if (x == null) {
            return new Node(key, val);
        }

        int cmp = key.compareTo(x.key);

        if (cmp < 0) {
            x.left = put(x.left, key, val);
        } else if (cmp > 0) {
            x.right = put(x.right, key, val);
        } else if (cmp == 0) {
            x.val = val;
        }

        return x;
    }

    public Iterable<Key> keys()
    {
        Queue<Key> q = new Queue<Key>();

        inorder(root, q);

        return q;
    }

    private void inorder(Node x, Queue<Key> q)
    {
        if (x == null) {
            return;
        }

        inorder(x.left, q);

        q.enqueue(x.key);

        inorder(x.right, q);
    }

    public void level_order_keys(int depth)
    {
        for (int i = 0; i < depth; ++i) {
            StdOut.print(level_order(root, i));
        }
    }

    private String level_order(Node x, int level)
    {
        if (x == null) {
            return "";
        }

        if (level == 0) {
            return x.key + " ";
        } else if (level > 0) {
            String leftStr = level_order(x.left, level - 1);
            String rightStr = level_order(x.right, level - 1);

            return leftStr + rightStr;
        } else {
          return "";
        }
    }

    private Node min(Node x) {
        if (x.left == null) {
            return x;
        } else {
            return min(x.left);
        }
    }

    public void deleteMin() {
        root = deleteMin(root);
    }

    private Node deleteMin(Node x) {
        if (x.left == null) {
            return x.right;
        }

        x.left = deleteMin(x.left);

        return x;
    }

    public void delete(Key key) {
        root = delete(root, key);
    }

    private Node delete(Node x, Key key) {
        if (x == null) {
            return null;
        }

        int cmp = key.compareTo(x.key);

        if (cmp < 0) {
            x.left = delete(x.left, key);
        } else if (cmp > 0) {
            x.right = delete(x.right, key);
        } else {
            if (x.right == null) {
                return x.left;
            }

            if (x.left == null) {
                return x.right;
            }

            Node t = x;
            x = min(t.right);
            x.right = deleteMin(t.right);
            x.left = t.left;
        }

        return x;
    }

    public static BST<Integer, Integer> fromArray(Integer[] array) {
        BST<Integer, Integer> value = new BST<Integer, Integer>();

        for (Integer key : array) {
            value.put(key, 0);
        }

        return value;
    }

    public static void main(String[] args) {
        BST<Integer, Integer> bst = BST.fromArray(new Integer[] { 59, 69, 37, 46, 51, 44, 23, 82, 93, 11 });

        bst.level_order_keys(5);
    }
}
