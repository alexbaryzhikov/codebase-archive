/* --------------------------------------------------------
 * Proxy
 * --------------------------------------------------------
 *
 * Intent
 * - provide a placeholder for an object to control references to it
 *
 * Applicability
 *
 * - Virtual Proxies: delaying the creation and initialization of expensive objects until needed
 * - Remote Proxies: providing a local representation for an object that is in a different address
 *   space
 * - Protection Proxies: where a proxy controls access to RealSubject methods, by giving access
 *   to some objects while denying access to others
 * - Smart References: providing a sophisticated access to certain objects such as tracking
 *   the number of references to an object and denying access if a certain number is reached,
 *   as well as loading an object from database into memory on demand
 *
 * Related Patterns
 *
 * - Adapter: implements a different interface to the object it adapts where a proxy implements
 *   the same interface as its subject
 * - Decorator: a decorator implementation can be the same as the proxy however a decorator adds
 *   responsibilities to an object while a proxy controls access to it
 */

public class Main {
  public static void main(String[] args) {
    // Assuming that the user selects a folder that has 3 images
    Image image1 = new ImageProxy("sample/hires_photo1.jpg");
    Image image2 = new ImageProxy("sample/hires_photo2.jpg");
    Image image3 = new ImageProxy("sample/hires_photo3.jpg");

    // Assume that the user clicks on one image at a time. This would cause the program to call
    // showImage() for that image only. In this case only one image is loaded into memory
    image1.showImage();

    // User selects another image
    image2.showImage();

    // And another
    image3.showImage();

    // Then goes back to first image, which is already cached in proxy
    image1.showImage();
  }
}
