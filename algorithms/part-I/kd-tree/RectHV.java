public class RectHV {
  private final double xmin, ymin;
  private final double xmax, ymax;

  public RectHV(double xmin, double ymin, double xmax, double ymax) {
    if (xmax < xmin || ymax < ymin) {
      throw new IllegalArgumentException("Invalid rectangle");
    }

    this.xmin = xmin;
    this.ymin = ymin;
    this.xmax = xmax;
    this.ymax = ymax;
  }

  public double xmin() {
    return xmin;
  }

  public double ymin() {
    return ymin;
  }

  public double xmax() {
    return xmax;
  }

  public double ymax() {
    return ymax;
  }

  public double width() {
    return xmax - xmin;
  }

  public double height() {
    return ymax - ymin;
  }

  public boolean intersects(RectHV that) {
    return this.xmax >= that.xmin && this.ymax >= that.ymin
        && that.xmax >= this.xmin && that.ymax >= this.ymin;
  }

  public void draw() {
    StdDraw.line(xmin, ymin, xmax, ymin);
    StdDraw.line(xmax, ymin, xmax, ymax);
    StdDraw.line(xmax, ymax, xmin, ymax);
    StdDraw.line(xmin, ymax, xmin, ymin);
  }

  public double distanceTo(Point2D p) {
    return Math.sqrt(this.distanceSquaredTo(p));
  }

  public double distanceSquaredTo(Point2D p) {
    double dx = 0.0, dy = 0.0;

    if (p.x() < xmin) {
      dx = p.x() - xmin;
    } else if (p.x() > xmax) {
      dx = p.x() - xmax;
    }

    if (p.y() < ymin) {
      dy = p.y() - ymin;
    } else if (p.y() > ymax) {
      dy = p.y() - ymax;
    }

    return dx * dx + dy * dy;
  }

  public boolean contains(Point2D p) {
    return (p.x() >= xmin)
        && (p.x() <= xmax)
        && (p.y() >= ymin)
        && (p.y() <= ymax);
  }

  @Override
  public boolean equals(Object y) {
    if (y == this) {
      return true;
    }

    if (y == null) {
      return false;
    }

    if (y.getClass() != this.getClass()) {
      return false;
    }

    RectHV that = (RectHV) y;

    if (this.xmin != that.xmin) {
      return false;
    }

    if (this.ymin != that.ymin) {
      return false;
    }

    if (this.xmax != that.xmax) {
      return false;
    }

    if (this.ymax != that.ymax) {
      return false;
    }

    return true;
  }

  @Override
  public String toString() {
    return "[" + xmin + ", " + xmax + "] x [" + ymin + ", " + ymax + "]";
  }
}
