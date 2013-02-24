import java.util.Iterator;
import java.util.NoSuchElementException;

public class RandomizedQueue<Item> implements Iterable<Item> {
    private int N;
    private Item[] array;

    private class RandomizedQueueIterator implements Iterator<Item> {
        private final RandomizedQueue<Item> elements;

        public RandomizedQueueIterator(RandomizedQueue<Item> container) {
            elements = new RandomizedQueue<Item>(container);
        }

        @Override
        public boolean hasNext() {
            return !elements.isEmpty();
        }

        @Override
        public Item next() {
            if (!hasNext()) {
                throw new NoSuchElementException("There is no next element");
            }

            return elements.dequeue();
        }

        @Override
        public void remove() {
            throw new UnsupportedOperationException("remove is unsupported");
        }
    }

    private RandomizedQueue(RandomizedQueue<Item> source) {
        this.array = source.array.clone();
        this.N = source.N;
    }

    // Construct an empty deque.
    public RandomizedQueue() {
        N = 0;
        array = (Item[]) new Object[2];
    }

    // Is the queue empty?
    public boolean isEmpty() {
        return N == 0;
    }

    // Return the number of items on the queue.
    public int size() {
        return N;
    }

    private void resize(int capacity) {
        assert capacity >= N;

        Item[] temp = (Item[]) new Object[capacity];

        for (int i = 0; i < N; i++) {
            temp[i] = array[i];
        }

        array = temp;
    }

    private int getPosition() {
        return StdRandom.uniform(size());
    }

    // Add the item.
    public void enqueue(Item item) {
        if (item == null) {
            throw new NullPointerException("Inserting a null item");
        }

        if (N == array.length) {
            resize(2 * array.length);
        }

        array[N++] = item;
    }

    // Delete and return a random item.
    public Item dequeue() {
        if (isEmpty()) {
            throw new NoSuchElementException("You cannot dequeue from empty queue");
        }

        int index = getPosition();

        // Get randomized item and swap it with last element.
        Item temp = array[index];
        array[index] = array[N - 1];
        array[N - 1] = temp;

        // Remove last.
        Item item = array[N - 1];
        array[N - 1] = null;
        N--;

        // Resize array if needed.
        if (N > 0 && N == (array.length / 4)) {
            resize(array.length / 2);
        }

        return item;
    }

    // Return (but do not delete) a random item.
    public Item sample() {
        if (isEmpty()) {
            throw new NoSuchElementException("You cannot sample from empty queue");
        }

        return array[getPosition()];
    }

    // Return an independent iterator over items in random order.
    @Override
    public Iterator<Item> iterator() {
        return new RandomizedQueueIterator(this);
    }

    public static void main(String[] args) {
        RandomizedQueue<Integer> queue = new RandomizedQueue<Integer>();

        for (int i = 0; i < 10; ++i) {
            queue.enqueue(i);
        }

        assert queue.size() == 10;
        assert !queue.isEmpty();

        for (int i = 0; i < 10; ++i) {
            StdOut.println(queue.sample());
        }

        assert queue.size() == 10;
        assert !queue.isEmpty();

        while (!queue.isEmpty()) {
            StdOut.print(queue.dequeue() + " ");
        }

        StdOut.println();
        StdOut.println();

        assert queue.size() == 0;
        assert queue.isEmpty();

        for (int i = 0; i < 10; ++i) {
            queue.enqueue(i);
        }

        for (int val: queue) {
            StdOut.print(val + " ");
        }

        assert queue.size() == 10;
        assert !queue.isEmpty();

        StdOut.println();

        for (int val: queue) {
            StdOut.print(val + " ");
        }

        assert queue.size() == 10;
        assert !queue.isEmpty();


        StdOut.println();
    }
}