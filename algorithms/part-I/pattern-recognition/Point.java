import java.util.Comparator;

public class Point implements Comparable<Point> {
    
    private class SlopeComparator implements Comparator<Point> {
        public int compare(Point a, Point b) {
            return 0;
        }
    }
    
    public final Comparator<Point> SLOPE_ORDER = new SlopeComparator();

    private final int x;
    private final int y;

    // Create the point (x, y).
    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    // Plot this point to standard drawing.
    public void draw() {
        StdDraw.point(x, y);
    }

    // Draw line between this point and that point to standard drawing.
    public void drawTo(Point that) {
        StdDraw.line(this.x, this.y, that.x, that.y);
    }

    // Slope between this point and that point.
    public double slopeTo(Point that) {
        // CODE HERE.
        return 0.0;
    }

    // Is this point lexicographically smaller than that one?
    // Comparing y-coordinates and breaking ties by x-coordinates.
    public int compareTo(Point that) {
        // CODE HERE.
        return 0;
    }

    // Return string representation of this point.
    public String toString() {
        return "(" + x + ", " + y + ")";
    }

    public static void main(String[] args) {
    }
}