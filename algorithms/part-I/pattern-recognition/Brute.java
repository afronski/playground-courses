public class Brute {
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

        for (int p = 0; p < N; ++p) {
            for (int q = 0; q < N; ++q) {
                if (p == q) {
                    continue;
                }

                for (int r = 0; r < N; ++r) {
                    if (p == r || q == r) {
                        continue;
                    }

                    for (int s = 0; s < N; ++s) {
                        if (p == s || q == s || r == s) {
                            continue;
                        }

                        double slopePQ = points[p].slopeTo(points[q]);
                        double slopePR = points[p].slopeTo(points[r]);
                        double slopePS = points[p].slopeTo(points[s]);

                        if (slopePQ == slopePR
                            && slopePQ == slopePS
                            && slopePR == slopePS) {
                            if (points[p].compareTo(points[q]) < 0
                                && points[q].compareTo(points[r]) < 0
                                && points[r].compareTo(points[s]) < 0) {
                                StdOut.println(
                                    points[p] + " -> "
                                  + points[q] + " -> "
                                  + points[r] + " -> "
                                  + points[s]);

                                points[p].drawTo(points[s]);
                            }
                        }
                    }
                }
            }
        }

        StdDraw.show(0);
    }
}