import java.awt.Color;
import java.awt.FileDialog;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;

import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.KeyStroke;

public final class Picture implements ActionListener {
  private BufferedImage image;
  private JFrame frame;
  private String filename;
  private boolean isOriginUpperLeft = true;
  private final int width, height;

  public Picture(int w, int h) {
    width = w;
    height = h;
    image = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB);

    filename = w + "-by-" + h;
  }

  public Picture(Picture picture) {
    width = picture.width();
    height = picture.height();

    image = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB);
    filename = picture.filename;

    for (int i = 0; i < width(); i++) {
      for (int j = 0; j < height(); j++) {
        image.setRGB(i, j, picture.get(i, j).getRGB());
      }
    }
  }

  public Picture(String filename) {
    this.filename = filename;

    try {
      File file = new File(filename);
      if (file.isFile()) {
        image = ImageIO.read(file);
      } else {
        URL url = getClass().getResource(filename);

        if (url == null) {
          url = new URL(filename);
        }

        image = ImageIO.read(url);
      }

      width = image.getWidth(null);
      height = image.getHeight(null);
    } catch (IOException e) {
      throw new RuntimeException("Could not open file: " + filename);
    }
  }

  public Picture(File file) {
    try {
      image = ImageIO.read(file);
    } catch (IOException e) {
      e.printStackTrace();

      throw new RuntimeException("Could not open file: " + file);
    }

    if (image == null) {
      throw new RuntimeException("Invalid image file: " + file);
    }

    width = image.getWidth(null);
    height = image.getHeight(null);
    filename = file.getName();
  }

  public JLabel getJLabel() {
    if (image == null) {
      return null;
    }

    ImageIcon icon = new ImageIcon(image);
    return new JLabel(icon);
  }

  public void setOriginUpperLeft() {
    isOriginUpperLeft = true;
  }

  public void setOriginLowerLeft() {
    isOriginUpperLeft = false;
  }

  public void show() {
    if (frame == null) {
      frame = new JFrame();

      JMenuBar menuBar = new JMenuBar();
      JMenu menu = new JMenu("File");

      menuBar.add(menu);
      JMenuItem menuItem1 = new JMenuItem(" Save...   ");

      menuItem1.addActionListener(this);
      menuItem1.setAccelerator(
                  KeyStroke.getKeyStroke(KeyEvent.VK_S,
                  Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
      menu.add(menuItem1);
      frame.setJMenuBar(menuBar);

      frame.setContentPane(getJLabel());

      frame.setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);
      frame.setTitle(filename);
      frame.setResizable(false);

      frame.pack();
      frame.setVisible(true);
    }

    frame.repaint();
  }

  public int height() {
    return height;
  }

  public int width() {
    return width;
  }

  public Color get(int i, int j) {
    if (isOriginUpperLeft) {
      return new Color(image.getRGB(i, j));
    } else {
      return new Color(image.getRGB(i, height - j - 1));
    }
  }

  public void set(int i, int j, Color c) {
    if (c == null) {
      throw new RuntimeException("can't set Color to null");
    }

    if (isOriginUpperLeft) {
      image.setRGB(i, j, c.getRGB());
    } else {
      image.setRGB(i, height - j - 1, c.getRGB());
    }
  }

  @Override
  public boolean equals(Object obj) {
    if (obj == this) {
      return true;
    }

    if (obj == null) {
      return false;
    }

    if (obj.getClass() != this.getClass()) {
      return false;
    }

    Picture that = (Picture) obj;

    if (this.width() != that.width()) {
      return false;
    }

    if (this.height() != that.height()) {
      return false;
    }

    for (int x = 0; x < width(); x++) {
      for (int y = 0; y < height(); y++) {
        if (!this.get(x, y).equals(that.get(x, y))) {
          return false;
        }
      }
    }

    return true;
  }

  public void save(String name) {
    save(new File(name));
  }

  public void save(File file) {
    this.filename = file.getName();

    if (frame != null) {
      frame.setTitle(filename);
    }

    String suffix = filename.substring(filename.lastIndexOf('.') + 1);
    suffix = suffix.toLowerCase();

    if (suffix.equals("jpg") || suffix.equals("png")) {
      try {
        ImageIO.write(image, suffix, file);
      } catch (IOException e) {
        e.printStackTrace();
      }
    } else {
      System.out.println("Error: filename must end in .jpg or .png");
    }
  }

  @Override
  public void actionPerformed(ActionEvent e) {
    FileDialog chooser = new FileDialog(
                              frame,
                              "Use a .png or .jpg extension",
                              FileDialog.SAVE);
    chooser.setVisible(true);

    if (chooser.getFile() != null) {
      save(chooser.getDirectory() + File.separator + chooser.getFile());
    }
  }

  public static void main(String[] args) {
    Picture picture = new Picture(args[0]);
    System.out.printf("%d-by-%d\n", picture.width(), picture.height());

    picture.show();
  }
}