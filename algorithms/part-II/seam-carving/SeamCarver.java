import java.awt.Color;

public class SeamCarver {
  private static final int BORDERENERGY = 195075;
  private static final double EPS = 1e-12;

  private Picture picture;

  private double[][] distance;

  public SeamCarver(Picture picture) {
    this.picture = picture;
  }

  public Picture picture() {
    return this.picture;
  }

  public int width() {
    return picture.width();
  }

  public int height() {
    return picture.height();
  }

  private double pow2(int value) {
    return 1.0 * value * value;
  }

  private double min(double x, double y) {
    if (x > y) {
      return y;
    } else {
      return x;
    }
  }

  private double min(double x, double y, double z) {
    return min(z, min(x, y));
  }

  private boolean equal(double x, double y) {
    return Math.abs(x - y) < EPS;
  }

  public double energy(int x, int y) {
    if (x < 0 || x > width() - 1 || y < 0 || y > height() - 1) {
      throw new IndexOutOfBoundsException();
    }

    if (x == 0 || x == width() - 1 || y == 0 || y == height() - 1) {
      return BORDERENERGY;
    }

    Color x1 = picture.get(x - 1, y),
          x2 = picture.get(x + 1, y),
          y1 = picture.get(x, y - 1),
          y2 = picture.get(x, y + 1);

    return pow2(x1.getRed() - x2.getRed())
         + pow2(x1.getGreen() - x2.getGreen())
         + pow2(x1.getBlue() - x2.getBlue())
         + pow2(y1.getRed() - y2.getRed())
         + pow2(y1.getGreen() - y2.getGreen())
         + pow2(y1.getBlue() - y2.getBlue());
  }

  public int[] findHorizontalSeam() {
    int[] result = new int[width()];

    distance = new double[height()][width()];

    for (int i = 0; i < height(); i++) {
      distance[i][0] = 0.0;
    }

    for (int i = 1; i < width() - 1; ++i) {
      for (int j = 1; j < height() - 1; ++j) {
        if (j == 1 && j == height() - 2) {
          distance[j][i] = distance[j][i - 1] + energy(i, j);
        } else if (j == 1) {
          distance[j][i] = min(distance[j][i - 1], distance[j + 1][i - 1]) + energy(i, j);
        } else if (j == height() - 2) {
          distance[j][i] = min(distance[j - 1][i - 1], distance[j][i - 1]) + energy(i, j);
        } else {
          distance[j][i] = min(distance[j - 1][i - 1], distance[j][i - 1], distance[j + 1][i - 1]) + energy(i, j);
        }
      }
    }

    double minimalDistance = Double.MAX_VALUE;
    int index = 0;

    for (int i = 1; i < height() - 1; ++i) {
      distance[i][width() - 1] = distance[i][width() - 2] + BORDERENERGY;

      if (distance[i][width() - 1] < minimalDistance) {
        minimalDistance = distance[i][width() - 1];
        index = i;
      }
    }

    for (int i = width() - 1; i >= 1; --i) {
      for (int j = 1; j <= height() - 2; ++j) {
        if (equal(minimalDistance, distance[j - 1][i]) && Math.abs(index - j + 1) <= 1) {
          result[i] = j - 1;
          minimalDistance -= energy(i, j - 1);
          index = j - 1;
          break;
        } else if (equal(minimalDistance, distance[j][i]) && Math.abs(index - j) <= 1) {
          result[i] = j;
          minimalDistance -= energy(i, j);
          index = j;
          break;
        } else if (equal(minimalDistance, distance[j + 1][i])  && Math.abs(index - j - 1) <= 1) {
          result[i] = j + 1;
          minimalDistance -= energy(i, j + 1);
          index = j + 1;
          break;
        }
      }
    }

    result[width() - 1] = result[width() - 2] - 1;
    result[0] = result[1] - 1;

    return result;
  }

  public int[] findVerticalSeam() {
    int[] result = new int[height()];
    distance = new double[height()][width()];

    for (int i = 0; i < width(); ++i) {
      distance[0][i] = 0.0;
    }

    for (int i = 1; i < height() - 1; ++i) {
      for (int j = 1; j < width() - 1; ++j) {
        if (j == 1 && j == width() - 2) {
          distance[i][j] = distance[i - 1][j] + energy(j, i);
        } else if (j == 1) {
          distance[i][j] = min(distance[i - 1][j], distance[i - 1][j + 1]) + energy(j, i);
        } else if (j == width() - 2) {
          distance[i][j] = min(distance[i - 1][j - 1], distance[i - 1][j]) + energy(j, i);
        } else {
          distance[i][j] = min(distance[i - 1][j - 1], distance[i - 1][j], distance[i - 1][j + 1]) + energy(j, i);
        }
      }
    }

    double minimalDistance = Double.MAX_VALUE;
    int index = 0;

    for (int i = 1; i < width() - 1; ++i) {
      distance[height() - 1][i] = distance[height() - 2][i] + BORDERENERGY;

      if (distance[height() - 1][i] < minimalDistance) {
        index = i;
        minimalDistance = distance[height() - 1][i];
      }
    }

    for (int i = height() - 1; i >= 1; --i) {
      for (int j = 1; j <= width() - 2; ++j) {
        if (equal(minimalDistance, distance[i][j - 1]) && Math.abs(index - j + 1) <= 1) {
          result[i] = j - 1;
          minimalDistance -= energy(j - 1, i);
          index = j - 1;
          break;
        } else if (equal(minimalDistance, distance[i][j]) && Math.abs(index - j) <= 1) {
          result[i] = j;
          minimalDistance -= energy(j, i);
          index = j;
          break;
        } else if (equal(minimalDistance, distance[i][j + 1]) && Math.abs(index - j - 1) <= 1) {
          result[i] = j + 1;
          minimalDistance -= energy(j + 1, i);
          index = j + 1;
          break;
        }
      }
    }

    result[height() - 1] = result[height() - 2] - 1;
    result[0] = result[1] - 1;

    return result;
  }

  public void removeHorizontalSeam(int[] seam) {
    if (height() <= 1 || seam.length != width()) {
      throw new IllegalArgumentException();
    }

    Picture modified = new Picture(width(), height() - 1);
    int old = seam[0];

    for (int i = 0; i < seam.length; ++i) {
      if (Math.abs(seam[i] - old) > 1) {
        throw new IllegalArgumentException();
      }

      for (int j = 0; j < seam[i]; ++j) {
        modified.set(i, j, picture.get(i, j));
      }

      for (int j = seam[i]; j < height() - 1; ++j) {
        modified.set(i, j, picture.get(i, j + 1));
      }

      old = seam[i];
    }

    picture = modified;
  }

  public void removeVerticalSeam(int[] seam) {
    if (width() <= 1 || seam.length != height()) {
      throw new IllegalArgumentException();
    }

    Picture modified = new Picture(width() - 1, height());
    int old = seam[0];

    for (int i = 0; i < seam.length; ++i) {
      if (Math.abs(seam[i] - old) > 1) {
        throw new IllegalArgumentException();
      }

      for (int j = 0; j < seam[i]; ++j) {
        modified.set(j, i, picture.get(j, i));
      }

      for (int j = seam[i]; j < width() - 1; ++j) {
        modified.set(j, i, picture.get(j + 1, i));
      }

      old = seam[i];
    }

    picture = modified;
  }
}