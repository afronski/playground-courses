import java.awt.Color;

public class SCUtility {
  public static Picture randomPicture(int W, int H) {
    Picture picture = new Picture(W, H);
    for (int i = 0; i < W; ++i) {
      for (int j = 0; j < H; ++j) {
        int r = StdRandom.uniform(255);
        int g = StdRandom.uniform(255);
        int b = StdRandom.uniform(255);

        Color c = new Color(r, g, b);
        picture.set(i, j, c);
      }
    }

    return picture;
  }

  public static double[][] toEnergyMatrix(SeamCarver carver) {
    double[][] returnDouble = new double[carver.width()][carver.height()];

    for (int i = 0; i < carver.width(); ++i) {
      for (int j = 0; j < carver.height(); ++j) {
        returnDouble[i][j] = carver.energy(i, j);
      }
    }

    return returnDouble;
  }

  public static void showEnergy(SeamCarver carver) {
    doubleToPicture(toEnergyMatrix(carver)).show();
  }

  public static Picture toEnergyPicture(SeamCarver carver) {
    double[][] energyMatrix = toEnergyMatrix(carver);

    return doubleToPicture(energyMatrix);
  }

  public static Picture doubleToPicture(double[][] grayValues) {
    int width = grayValues.length;
    int height = grayValues[0].length;

    Picture picture = new Picture(width, height);

    double maxVal = 0;

    for (int i = 0; i < width; ++i) {
      for (int j = 0; j < height; ++j) {
        if (grayValues[i][j] > maxVal) {
          maxVal = grayValues[i][j];
        }
      }
    }

    if (maxVal == 0) {
      return picture;
    }

    for (int i = 0; i < width; ++i) {
      for (int j = 0; j < height; ++j) {
        float normalizedGrayValue = (float) grayValues[i][j] / (float) maxVal;

        picture.set(i, j, new Color(normalizedGrayValue, normalizedGrayValue, normalizedGrayValue));
      }
    }

    return picture;
  }

  public static Picture seamOverlay(Picture picture, boolean horizontal, int[] seamIndices) {
    Picture overlaid = new Picture(picture.width(), picture.height());

    for (int i = 0; i < picture.width(); ++i) {
      for (int j = 0; j < picture.height(); ++j) {
        overlaid.set(i, j, picture.get(i, j));
      }
    }

    int width = picture.width();
    int height = picture.height();

    if (horizontal) {
      for (int i = 0; i < width; ++i) {
        overlaid.set(i, seamIndices[i], new Color(255, 0, 0));
      }
    } else {
      for (int j= 0; j < height; ++j) {
        overlaid.set(seamIndices[j], j, new Color(255, 0, 0));
      }
    }

    return overlaid;
  }
}