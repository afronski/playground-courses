import java.util.Iterator;
import java.util.NoSuchElementException;

public class Deque<Item> implements Iterable<Item> {

    private Node<Item> tail;
    private Node<Item> head;

    // Construct an empty deque.
    public Deque() {
        tail = null;
        head = null;
    }

    @SuppressWarnings("hiding")
    private class Node<Item> {
        private Node<Item> next;
        private Node<Item> prev;

        private final Item value;

        public Node(Item item) {
            value = item;
        }

        public Item getValue() {
            return value;
        }
    }

    private class DequeIterator implements Iterator<Item> {
        private Node<Item> current = head;

        @Override
        public boolean hasNext() {
            return current != null;
        }

        @Override
        public Item next() {
            if (!hasNext()) {
                throw new NoSuchElementException("There is no next element");
            }

            Item element = current.value;
            current = current.next;

            return element;
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("remove is unsupported");
        }
    }

    private void handleEmptyDeque(Item value) {
        Node<Item> newItem = new Node<Item>(value);
        head = newItem;
        tail = newItem;
    }

    // Is the deque empty?
    public boolean isEmpty() {
        return head == null && tail == null;
    }

    // Return the number of items on the deque.
    public int size() {
        int length = 0;
        Node<Item> current = head;

        if (current != null) {
            ++length;

            while (current.next != null) {
                current = current.next;
                ++length;
            }
        }

        return length;
    }

    // Insert the item at the front.
    public void addFirst(Item item) {
        if (item == null) {
            throw new NullPointerException("Inserting a null item");
        }

        if (isEmpty()) {
            handleEmptyDeque(item);
        } else {
            Node<Item> oldHead = head;
            head = new Node<Item>(item);

            oldHead.prev = head;
            head.next = oldHead;
        }
    }

    // Insert the item at the end.
    public void addLast(Item item) {
        if (item == null) {
            throw new NullPointerException("Inserting a null item");
        }

        if (isEmpty()) {
            handleEmptyDeque(item);
        } else {
            Node<Item> oldTail = tail;
            tail = new Node<Item>(item);

            oldTail.next = tail;
            tail.prev = oldTail;
        }
    }

    // Delete and return the item at the front.
    public Item removeFirst() {
        if (isEmpty()) {
            throw new NoSuchElementException("Deque is empty");
        }

        Node<Item> oldHead = head;
        Item value = oldHead.getValue();

        if (head == tail) {
            head = null;
            tail = null;
        } else {
            head = head.next;
            head.prev = null;
        }

        oldHead = null;

        return value;
    }

    // Delete and return the item at the end.
    public Item removeLast() {
        if (isEmpty()) {
            throw new NoSuchElementException("Deque is empty");
        }

        Node<Item> oldTail = tail;
        Item value = oldTail.getValue();

        if (head == tail) {
            head = null;
            tail = null;
        } else {
            tail = tail.prev;
            tail.next = null;
        }

        return value;
    }

    // Return an iterator over items in order from front to end.
    @Override
    public Iterator<Item> iterator() {
        return new DequeIterator();
    }

    public static void main(String[] args) {
        Deque<Integer> ints = new Deque<Integer>();

        assert ints.size() == 0;
        assert ints.isEmpty();

        ints.addFirst(1);
        assert ints.size() == 1;
        assert !ints.isEmpty();

        int val = ints.removeFirst();
        assert val == 1;
        assert ints.size() == 0;
        assert ints.isEmpty();

        ints.addFirst(2);
        assert ints.size() == 1;
        assert !ints.isEmpty();

        val = ints.removeLast();
        assert val == 2;
        assert ints.size() == 0;
        assert ints.isEmpty();

        ints.addFirst(3);
        ints.addFirst(2);
        ints.addFirst(1);
        ints.addLast(4);
        ints.addLast(5);
        ints.addLast(6);

        assert ints.size() == 6;
        assert !ints.isEmpty();

        for (int value : ints) {
            StdOut.print(value + " ");
        }

        StdOut.println();

        while (!ints.isEmpty()) {
            StdOut.println(ints.removeLast());
        }

        assert ints.size() == 0;
        assert ints.isEmpty();

        for (int i = 0; i < 10; ++i) {
            ints.addFirst(i);
        }

        for (int i = 0; i < 10; ++i) {
            assert ints.size() == 10 - i;
            ints.removeLast();
        }

        assert ints.size() == 0;
    }
}