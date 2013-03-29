public class QuickSort {
    public static void exch(Comparable[] a, int i, int j) {
        Comparable temp = a[i];
        a[i] = a[j];
        a[j] = temp;
    }

    public static boolean less(Comparable a, Comparable b) {
        return a.compareTo(b) < 0;
    }

    public static int partition(Comparable[] a, int lo, int hi) {
        int i = lo, j = hi + 1;

        while (true) {
            while (less(a[++i], a[lo])) {
                if (i == hi) {
                    break;
                }
            }

            while (less(a[lo], a[--j])) {
                if (j == lo) {
                    break;
                }
            }

            if (i >= j) {
                break;
            }

            exch(a, i, j);
        }

        exch(a, lo, j);
        return j;
    }

    public static int partitionDijkstra(Comparable[] a, int lo, int hi) {
        int lt = lo, gt = hi;
        Comparable v = a[lo];
        int i = 0;

        while (i <= gt) {
            int cmp = a[i].compareTo(v);
            if      (cmp < 0) exch(a, lt++, i++);
            else if (cmp > 0) exch(a, i, gt--);
            else              i++;
        }

        return lt;
    }

    public static void main(String[] args) {
        Comparable[] integers = new Comparable[] { 33, 34, 77, 15, 45, 72, 80, 94, 29, 26, 32, 91 };
        partition(integers, 0, integers.length - 1);

        for(Comparable in: integers) {
            StdOut.print(in + " ");
        }

        StdOut.println();
    }
}