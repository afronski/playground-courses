import java.util.Arrays;

public class Fast {
    private static void swap(Point[] array, int from, int to) {
        Point temp = array[from];
        array[from] = array[to];
        array[to] = temp;
    }

    public static void main(String[] args) {
        String filename = args[0];
        In in = new In(filename);

        int N = in.readInt();

        StdDraw.setXscale(0, 32768);
        StdDraw.setYscale(0, 32768);
        StdDraw.show(0);

        Point[] points = new Point[N];

        for (int i = 0; i < N; i++) {
            int x = in.readInt();
            int y = in.readInt();

            points[i] = new Point(x, y);
            points[i].draw();
        }

        for (int i = 0; i < N; ++i) {
            Arrays.sort(points);

            Point p = points[i];

            swap(points, 0, i);

            Arrays.sort(points, 1, N, p.SLOPE_ORDER);

            for (int k = 1; k < N; ++k) {
                Point[] collinear = new Point[N];
                Point q = points[k];
                double slope = p.slopeTo(q);

                int collinearSize = 2;
                collinear[0] = p;
                collinear[1] = q;

                for (int j = k + 1; j < N; ++j) {
                    if (slope == p.slopeTo(points[j])) {
                        collinear[collinearSize] = points[j];
                        ++collinearSize;
                    }
                }

                if (collinearSize >= 4) {
                    boolean duplicate = false;

                    for (int j = 0; j < collinearSize; ++j) {
                        if (collinear[j].compareTo(p) < 0) {
                            duplicate = true;
                            break;
                        }
                    }

                    if (!duplicate) {
                        String representation = new String();
                        for (int j = 0; j < collinearSize; ++j) {
                            representation += collinear[j].toString();

                            if (j != (collinearSize - 1)) {
                                representation += " -> ";
                            }
                        }

                        StdOut.println(representation);
                        p.drawTo(collinear[collinearSize - 1]);
                    }
                }
            }

            swap(points, i, 0);
        }


        StdDraw.show(0);
    }
}