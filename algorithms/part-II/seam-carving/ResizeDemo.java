public class ResizeDemo {
  public static void main(String[] args) {
    if (args.length != 3) {
      System.out.println("Usage:\njava ResizeDemo [image filename] [num cols to remove] [num rows to remove]");
      return;
    }

    Picture inputImg = new Picture(args[0]);

    int removeColumns = Integer.parseInt(args[1]);
    int removeRows = Integer.parseInt(args[2]);

    System.out.printf("image is %d columns by %d rows\n", inputImg.width(), inputImg.height());
    SeamCarver carver = new SeamCarver(inputImg);

    Stopwatch stopwatch = new Stopwatch();

    for (int i = 0; i < removeRows; i++) {
      int[] horizontalSeam = carver.findHorizontalSeam();

      carver.removeHorizontalSeam(horizontalSeam);
    }

    for (int i = 0; i < removeColumns; i++) {
      int[] verticalSeam = carver.findVerticalSeam();

      carver.removeVerticalSeam(verticalSeam);
    }

    Picture outputImg = carver.picture();

    System.out.printf("new image size is %d columns by %d rows\n", carver.width(), carver.height());
    System.out.println("Resizing time: " + stopwatch.elapsedTime() + " seconds.");

    inputImg.show();
    outputImg.show();
  }
}
