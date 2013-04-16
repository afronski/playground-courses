import java.awt.Color;

public class SeamCarver {
  private static final double BORDER_ENERGY = 195075.0;
  private final Picture picture;

  public SeamCarver(Picture picture) {
    this.picture = new Picture(picture);
  }

  private double calculateDerivative(Color first, Color second) {
    int r = first.getRed() - second.getRed(),
        g = first.getGreen() - second.getGreen(),
        b = first.getBlue() - second.getBlue();

    return r * r + g * g + b * b;
  }

  private void validate(int x, int y) {
    if (x < 0 || x >= width()) {
      throw new IndexOutOfBoundsException("X value out of bounds.");
    }

    if (y < 0 || y >= height()) {
      throw new IndexOutOfBoundsException("Y value out of bounds.");
    }
  }

  private void validateSeam(int seamLength, int imageDimmension) {
    if (imageDimmension <= 1) {
      throw new IllegalArgumentException("Image dimmension is less or equal 1");
    }

    if (seamLength != imageDimmension) {
      throw new IllegalArgumentException("Seam length != Image dimmension");
    }
  }

  public Picture picture() {
    return picture;
  }

  public int width() {
    return picture.width();
  }

  public int height() {
    return picture.height();
  }

  public double energy(int x, int y) {
    double horizontalDerivative = 0.0,
           verticalDerivative = 0.0;

    validate(x, y);

    if (x == 0 || x == width() - 1) {
      return BORDER_ENERGY;
    }

    if (y == 0 || y == height() - 1) {
      return BORDER_ENERGY;
    }

    Color left = picture.get(x - 1, y),
          right = picture.get(x + 1, y);

    horizontalDerivative = calculateDerivative(left, right);

    Color up = picture.get(x, y - 1),
          down = picture.get(x, y + 1);

    verticalDerivative = calculateDerivative(up, down);

    return horizontalDerivative + verticalDerivative;
  }

  public int[] findHorizontalSeam() {
    return null;
  }

  public int[] findVerticalSeam() {
    return null;
  }

  public void removeHorizontalSeam(int[] seam) {
    validateSeam(seam.length, width());
  }

  public void removeVerticalSeam(int[] seam) {
    validateSeam(seam.length, height());
  }
}