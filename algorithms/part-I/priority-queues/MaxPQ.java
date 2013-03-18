

public class MaxPQ<Key extends Comparable<Key>> {
    private int N;
    private final Key[] heap;

    @SuppressWarnings("unchecked")
    public MaxPQ(int capacity) {
        heap = (Key[]) new Comparable[capacity + 1];
        N = 0;
    }

    public boolean isEmpty() {
        return N == 0;
    }

    public Key delMax() {
        Key max = heap[1];

        exch(1, N--);
        sink(1);
        heap[N + 1] = null;

        return max;
    }

    public void insert(Key toInsert) {
        heap[++N] = toInsert;
        swim(N);
    }

    private void swim(int k) {
        while (k > 1 && less(k / 2, k)) {
            exch(k, k / 2);
            k = k / 2;
        }
    }

    private void sink(int k) {
        while(2 * k <= N) {
            int j = 2 * k;

            if (j < N && less(j, j + 1)) {
                j++;
            }

            if (!less(k, j)) {
                break;
            }

            exch(k, j);
            k = j;
        }
    }

    private boolean less(int left, int right) {
        return heap[left].compareTo(heap[right]) < 0;
    }

    private void exch(int from, int to) {
        Key temp = heap[to];
        heap[to] = heap[from];
        heap[from] = temp;
    }

    public void print() {
        for (Key key: heap) {
            StdOut.print(key + " ");
        }

        StdOut.println();
    }

    public void setHeap(Key[] newHeap) {
        System.arraycopy(newHeap, 0, heap, 0, newHeap.length);
        N = newHeap.length - 1;
    }

    public void heapSortFirstPass() {
        for(int k = N / 2; k >= 1; --k) {
            sink(k);
        }
    }

    public static void main(String[] args) {
        MaxPQ<Integer> pq = new MaxPQ<Integer>(12);

        pq.setHeap(new Integer [] { 88, 83, 69, 60, 72, 28, 58, 27, 40, 54 });

        pq.insert(82);
        pq.insert(67);
        pq.insert(23);

        pq.print();
    }
}
