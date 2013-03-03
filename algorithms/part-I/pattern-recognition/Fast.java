import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

public class Fast {
    private class Result {
        private final int start;
        private final int end;
        private final double slope;

        public Result(int s, int e, double sl) {
            start = s;
            end = e;
            slope = sl;
        }

        public int getStart() {
            return start;
        }

        public int getEnd() {
            return end;
        }

        public double getSlope() {
            return slope;
        }
    }

    private final int threshold = 3;
    private final HashMap<Point, ArrayList<Double>> explored =
                   new HashMap<Point, ArrayList<Double>>();

    private static void printSegment(Point[] segment) {
        StdOut.print(segment[0]);

        for (int i = 1; i < segment.length; ++i) {
            StdOut.print(" -> " + segment[i]);
        }

        StdOut.println();
    }

    private static void drawSegment(Point[] segment) {
        segment[0].drawTo(segment[segment.length - 1]);
    }

    private ArrayList<Result> getSegments(Point origin, Point[] ordered) {
        int start = 0, end = 0;
        double slopePrevious = 0.0, slopeCurrent = 0.0;

        ArrayList<Result> list = new ArrayList<Result>();

        for (int index = 2; index < ordered.length; ++index) {
            slopePrevious = origin.slopeTo(ordered[index - 1]);
            slopeCurrent = origin.slopeTo(ordered[index]);

            if (slopePrevious == slopeCurrent) {
                end = index;
            } else {
                if ((end - start + 1) >= threshold) {
                    list.add(new Result(start, end, slopePrevious));
                }

                start = index;
                end = index;
            }
        }

        if ((end - start + 1) >= threshold) {
            list.add(new Result(start, end, slopePrevious));
        }

        return list;
    }

    private void printSegments(Point origin, Point[] ordered) {
        ArrayList<Result> results = this.getSegments(origin, ordered);

        for (Result result: results) {
            int length = result.getEnd() - result.getStart() + 1;
            Point[] segment = new Point[length + 1];

            for (int index = result.getStart();
                     index <= result.getEnd();
                     ++index) {
                segment[index - result.getStart()] = ordered[index];
            }

            segment[segment.length - 1] = origin;

            Arrays.sort(segment);
            Point lastPoint = segment[segment.length - 1];

            boolean beginExplored = this.explored.containsKey(segment[0])
                     && this.explored.get(segment[0]).contains(result.getSlope());
            boolean endExplored = this.explored.containsKey(lastPoint)
                     && this.explored.get(lastPoint).contains(result.getSlope());

            if (beginExplored || endExplored) {
                continue;
            }

            ArrayList<Double> slopes = new ArrayList<Double>();
            if (this.explored.containsKey(segment[0])) {
                slopes = this.explored.get(segment[0]);
            }

            slopes.add(result.getSlope());
            this.explored.put(segment[0], slopes);

            slopes = new ArrayList<Double>();
            if (this.explored.containsKey(lastPoint)) {
                slopes = this.explored.get(lastPoint);
            }

            slopes.add(result.getSlope());
            this.explored.put(lastPoint, slopes);

            drawSegment(segment);
            printSegment(segment);
        }
    }

    public static void main(String[] args) {
        Fast method = new Fast();

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

        Arrays.sort(points);

        for (int i = 0; i < N; ++i) {
            Point referencePoint = points[i];
            Point[] slopes = new Point[N];

            System.arraycopy(points, 0, slopes, 0, N);
            Arrays.sort(slopes, referencePoint.SLOPE_ORDER);

            method.printSegments(referencePoint, slopes);
        }

        StdDraw.show(0);
    }
}