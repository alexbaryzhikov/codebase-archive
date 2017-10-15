/**
 * Proxy
 */
public class ImageProxy implements Image {

  /** Private Proxy data*/
  private String imagePath;

  /** Reference to real subject*/
  private Image image;

  public ImageProxy(String imagePath) {
    this.imagePath = imagePath;
  }

  @Override
  public void showImage() {
    // Create image object only when image is required to be shown
    if (image == null) {
      image = new HighResImage(imagePath);
    }
    // Call showImage on real subject
    image.showImage();
  }
}
