public class PrintEnergy {
  public static void main(String[] args) {
    Picture inputImg = new Picture(args[0]);
    System.out.printf("image is %d pixels wide by %d pixels high.\n", inputImg.width(), inputImg.height());

    SeamCarver carver = new SeamCarver(inputImg);
    System.out.printf("Printing energy calculated for each pixel.\n");

    for (int j = 0; j < carver.height(); j++) {
      for (int i = 0; i < carver.width(); i++) {
        System.out.printf("%9.0f ", carver.energy(i, j));
      }

      System.out.println();
    }
  }
}