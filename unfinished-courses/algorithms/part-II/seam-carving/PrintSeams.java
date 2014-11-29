public class PrintSeams {
  private static void printHorizontalSeam(SeamCarver carver) {
    double totalSeamEnergy = 0;
    int[] horizontalSeam = carver.findHorizontalSeam();

    for (int j = 0; j < carver.height(); ++j) {
      for (int i = 0; i < carver.width(); ++i) {
        char lMarker = ' ',
             rMarker = ' ';

        if (j == horizontalSeam[i]) {
          lMarker = '[';
          rMarker = ']';

          totalSeamEnergy += carver.energy(i, j);
        }

        System.out.printf("%c%6.0f%c ", lMarker, carver.energy(i, j), rMarker);
      }

      System.out.println();
    }

    System.out.printf("\nTotal energy: %.0f\n\n", totalSeamEnergy);
  }


  private static void printVerticalSeam(SeamCarver carver) {
    double totalSeamEnergy = 0;
    int[] verticalSeam = carver.findVerticalSeam();

    for (int j = 0; j < carver.height(); ++j) {
      for (int i = 0; i < carver.width(); ++i) {
        char lMarker = ' ',
             rMarker = ' ';

        if (i == verticalSeam[j]) {
          lMarker = '[';
          rMarker = ']';

          totalSeamEnergy += carver.energy(i, j);
        }

        System.out.printf("%c%6.0f%c ", lMarker, carver.energy(i, j), rMarker);
      }

      System.out.println();
    }

    System.out.printf("\nTotal energy: %.0f\n\n", totalSeamEnergy);
  }

  public static void main(String[] args) {
    Picture inputImg = new Picture(args[0]);
    System.out.printf("image is %d columns by %d rows\n", inputImg.width(), inputImg.height());

    SeamCarver carver = new SeamCarver(inputImg);

    System.out.printf("Displaying horizontal seam calculated.\n");
    printHorizontalSeam(carver);

    System.out.printf("Displaying vertical seam calculated.\n");
    printVerticalSeam(carver);
  }
}