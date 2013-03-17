public class PointSET {
  private final SET<Point2D> points;

  public PointSET() {
    points = new SET<Point2D>();
  }

  public boolean isEmpty() {
    return points.isEmpty();
  }

  public int size() {
    return points.size();
  }

  public void insert(Point2D point) {
    if (!points.contains(point)) {
      points.add(point);
    }
  }

  public boolean contains(Point2D point) {
    return points.contains(point);
  }

  public void draw() {
    for (Point2D point : points) {
      point.draw();
    }
  }

  public Iterable<Point2D> range(RectHV rectangle) {
    Queue<Point2D> queue = new Queue<Point2D>();

    for (Point2D p : points) {
      if (rectangle.contains(p)) {
        queue.enqueue(p);
      }
    }

    return queue;
  }

  public Point2D nearest(Point2D point) {
    double minimalDistance = Double.MAX_VALUE;
    Point2D nearest = null;

    for (Point2D a : points) {
      double distance = point.distanceTo(a);

      if (distance < minimalDistance) {
        minimalDistance = distance;
        nearest = a;
      }
    }

    return nearest;
  }
}