/**
 * Real subject
 */
public class HighResImage implements Image {

  private String imagePath;

  public HighResImage(String imagePath) {
    this.imagePath = imagePath;
    loadImage(imagePath);
  }

  private void loadImage(String imagePath) {
    // Load image - heavy and costly operation
    System.out.println("HighResImage::loadImage " + imagePath);
  }

  @Override
  public void showImage() {
    System.out.println("HighResImage::showImage " + imagePath);
  }
}
