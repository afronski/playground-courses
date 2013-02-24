import java.util.Comparator;

public class Point implements Comparable<Point> {

    private class SlopeComparator implements Comparator<Point> {
        @Override
        public int compare(Point a, Point b) {
            double slopeA = slopeTo(a);
            double slopeB = slopeTo(b);

            if (slopeA == slopeB) {
                return 0;
            }

            if (slopeA < slopeB) {
                return -1;
            }

            return 1;
        }
    }

    public final Comparator<Point> SLOPE_ORDER = new SlopeComparator();

    private final int x;
    private final int y;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public void draw() {
        StdDraw.point(x, y);
    }

    // Draw line between this point and that point to standard drawing.
    public void drawTo(Point that) {
        StdDraw.line(this.x, this.y, that.x, that.y);
    }

    // Slope between this point and that point.
    public double slopeTo(Point that) {
        double a = that.y - this.y;
        double b = that.x - this.x;

        if (a == 0 && b == 0) {
            return Double.NEGATIVE_INFINITY;
        }

        if (a == 0) {
            return a;
        }

        if (b == 0) {
            return Double.POSITIVE_INFINITY;
        }

        return a / b;
    }

    // Is this point lexicographically smaller than that one?
    // Comparing y-coordinates and breaking ties by x-coordinates.
    @Override
    public int compareTo(Point that) {
        if (this.y > that.y) {
            return 1;
        } else {
            if (this.y == that.y) {
                if (this.x > that.x) {
                    return 1;
                } else if (this.x == that.x) {
                    return 0;
                }
            }
        }

        return -1;
    }

    @Override
    public String toString() {
        return "(" + x + ", " + y + ")";
    }

    public static void main(String[] args) {
    }
}