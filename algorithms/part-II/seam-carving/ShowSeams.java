public class ShowSeams {
  private static void showHorizontalSeam(SeamCarver carver) {
    Picture ep = SCUtility.toEnergyPicture(carver);
    int[] horizontalSeam = carver.findHorizontalSeam();

    Picture epOverlay = SCUtility.seamOverlay(ep, true, horizontalSeam);
    epOverlay.show();
  }

  private static void showVerticalSeam(SeamCarver carver) {
    Picture ep = SCUtility.toEnergyPicture(carver);
    int[] verticalSeam = carver.findVerticalSeam();

    Picture epOverlay = SCUtility.seamOverlay(ep, false, verticalSeam);
    epOverlay.show();
  }

  public static void main(String[] args) {
    Picture image = new Picture(args[0]);

    System.out.printf("image is %d columns by %d rows\n", image.width(), image.height());
    image.show();

    SeamCarver carver = new SeamCarver(image);

    System.out.printf("Displaying horizontal seam calculated.\n");
    showHorizontalSeam(carver);

    System.out.printf("Displaying vertical seam calculated.\n");
    showVerticalSeam(carver);
  }
}