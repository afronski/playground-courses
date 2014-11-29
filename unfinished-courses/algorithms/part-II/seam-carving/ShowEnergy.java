public class ShowEnergy {
  public static void main(String[] args) {
    Picture image = new Picture(args[0]);
    System.out.printf("image is %d columns by %d rows\n", image.width(), image.height());

    image.show();

    SeamCarver carver = new SeamCarver(image);

    System.out.printf("Displaying energy calculated for each pixel.\n");
    SCUtility.showEnergy(carver);
  }
}