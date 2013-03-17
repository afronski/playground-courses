public class KdTree {
  private static final double X_MINIMUM = 0;
  private static final double Y_MINIMUM = 0;

  private static final double X_MAXIMUM = 1;
  private static final double Y_MAXIMUM = 1;

  private Node root;
  private int size;

  private static class Node {
    private final Point2D point;
    private final RectHV rectangle;

    private Node leftBottom;
    private Node rightTop;

    public Node(Point2D point, RectHV rectangle) {
      this.point = point;
      this.rectangle = rectangle;
    }
  }

  public KdTree() {
    root = null;
    size = 0;
  }

  public boolean isEmpty() {
    return size == 0;
  }

  public int size() {
    return size;
  }

  public void insert(Point2D point) {
    if (isEmpty()) {
      RectHV rectangle = new RectHV(
                              X_MINIMUM,
                              Y_MINIMUM,
                              X_MAXIMUM,
                              Y_MAXIMUM);

      root = new Node(point, rectangle);
      ++size;
    } else if (!contains(point)) {
      compareInsertX(point, root);
      ++size;
    }
  }

  private void compareInsertX(Point2D p, Node node) {
    int result = Point2D.X_ORDER.compare(p, node.point);

    if (result < 0) {
      if (node.leftBottom == null) {
        RectHV rectangle = new RectHV(
                                node.rectangle.xmin(),
                                node.rectangle.ymin(),
                                node.point.x(),
                                node.rectangle.ymax());

        node.leftBottom = new Node(p, rectangle);
      } else {
        compareInsertY(p, node.leftBottom);
      }
    } else {
      if (node.rightTop == null) {
        RectHV rectangle = new RectHV(
                                node.point.x(),
                                node.rectangle.ymin(),
                                node.rectangle.xmax(),
                                node.rectangle.ymax());

        node.rightTop = new Node(p, rectangle);
      } else {
        compareInsertY(p, node.rightTop);
      }
    }
  }

  private void compareInsertY(Point2D p, Node node) {
    int result = Point2D.Y_ORDER.compare(p, node.point);

    if (result < 0) {
      if (node.leftBottom == null) {
        RectHV rectangle = new RectHV(
                                node.rectangle.xmin(),
                                node.rectangle.ymin(),
                                node.rectangle.xmax(),
                                node.point.y());

        node.leftBottom = new Node(p, rectangle);
      } else {
        compareInsertX(p, node.leftBottom);
      }
    } else {
      if (node.rightTop == null) {
        RectHV rectangle = new RectHV(
                                node.rectangle.xmin(),
                                node.point.y(),
                                node.rectangle.xmax(),
                                node.rectangle.ymax());

        node.rightTop = new Node(p, rectangle);
      } else {
        compareInsertX(p, node.rightTop);
      }
    }
  }

  private boolean compareFindX(Point2D p, Node node) {
    boolean found = false;

    if (p.equals(node.point)) {
      found = true;
    } else {
      int result = Point2D.X_ORDER.compare(p, node.point);

      if (result < 0) {
        if (node.leftBottom != null) {
          found = compareFindY(p, node.leftBottom);
        }
      } else {
        if (node.rightTop != null) {
          found = compareFindY(p, node.rightTop);
        }
      }
    }

    return found;
  }

  private boolean compareFindY(Point2D p, Node node) {
    boolean found = false;

    if (p.equals(node.point)) {
      found = true;
    } else {
      int result = Point2D.Y_ORDER.compare(p, node.point);

      if (result < 0) {
        if (node.leftBottom != null) {
          found = compareFindX(p, node.leftBottom);
        }
      } else {
        if (node.rightTop != null) {
          found = compareFindX(p, node.rightTop);
        }
      }
    }

    return found;
  }

  public boolean contains(Point2D p) {
    if (isEmpty()) {
      return false;
    } else {
      return compareFindX(p, root);
    }
  }

  public void draw() {
    if (isEmpty()) {
      return;
    }

    drawNode(root, true);
  }

  private void drawNode(Node node, boolean vertical) {
    if (node.leftBottom != null) {
      drawNode(node.leftBottom, !vertical);
    }

    StdDraw.setPenColor(StdDraw.BLACK);
    StdDraw.setPenRadius(.01);
    node.point.draw();

    StdDraw.setPenRadius();
    if (vertical) {
      StdDraw.setPenColor(StdDraw.RED);
      StdDraw.line(
                node.point.x(),
                node.rectangle.ymin(),
                node.point.x(),
                node.rectangle.ymax());
    } else {
      StdDraw.setPenColor(StdDraw.BLUE);
      StdDraw.line(
                node.rectangle.xmin(),
                node.point.y(),
                node.rectangle.xmax(),
                node.point.y());
    }

    if (node.rightTop != null) {
      drawNode(node.rightTop, !vertical);
    }
  }

  public Iterable<Point2D> range(RectHV rectangle) {
    Queue<Point2D> queue = new Queue<Point2D>();

    if (!isEmpty()) {
      queuePointsInRange(queue, rectangle, root);
    }

    return queue;
  }

  private void queuePointsInRange(Queue<Point2D> queue,
                                    RectHV rectangle,
                                    Node node) {

    if (node.leftBottom != null
     && rectangle.intersects(node.leftBottom.rectangle)) {
      queuePointsInRange(queue, rectangle, node.leftBottom);
    }

    if (rectangle.contains(node.point)) {
      queue.enqueue(node.point);
    }

    if (node.rightTop != null
     && rectangle.intersects(node.rightTop.rectangle)) {
      queuePointsInRange(queue, rectangle, node.rightTop);
    }
  }

  public Point2D nearest(Point2D point) {
    Point2D closestPoint = null;

    if (!isEmpty()) {
      closestPoint = nearest(point, root.point, root, true);
    }

    return closestPoint;
  }

  private Point2D nearest(Point2D point,
                           Point2D closest,
                           Node node,
                           boolean vertical) {

    Point2D result = closest;

    if (!node.point.equals(closest)
     && point.distanceSquaredTo(node.point)
        < point.distanceSquaredTo(closest)) {
      result = node.point;
    }

    if (node.leftBottom != null && node.rightTop != null) {
      if (vertical) {
        if (point.x() < node.point.x()) {
          if (node.leftBottom.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.leftBottom, !vertical);
          }
          if (node.rightTop.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.rightTop, !vertical);
          }
        } else {
          if (node.rightTop.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.rightTop, !vertical);
          }
          if (node.leftBottom.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.leftBottom, !vertical);
          }
        }
      } else {
        if (point.y() < node.point.y()) {
          if (node.leftBottom.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.leftBottom, !vertical);
          }
          if (node.rightTop.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.rightTop, !vertical);
          }
        } else {
          if (node.rightTop.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.rightTop, !vertical);
          }
          if (node.leftBottom.rectangle.distanceSquaredTo(point)
              < point.distanceSquaredTo(closest)) {
            result = nearest(point, closest, node.leftBottom, !vertical);
          }
        }
      }
    } else {
      if (node.leftBottom != null
       && node.leftBottom.rectangle.distanceSquaredTo(point)
          < point.distanceSquaredTo(closest)) {
        result = nearest(point, closest, node.leftBottom, !vertical);
      }

      if (node.rightTop != null
       && node.rightTop.rectangle.distanceSquaredTo(point)
          < point.distanceSquaredTo(closest)) {
        result = nearest(point, closest, node.rightTop, !vertical);
      }
    }

    return result;
  }
}