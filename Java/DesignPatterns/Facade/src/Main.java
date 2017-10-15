/* --------------------------------------------------------
 * Facade
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Hide the complexities of the system and provide an interface using which the client can
 *   access the system
 *
 * This pattern involves a single class which provides simplified methods required by client
 * and delegates calls to methods of existing system classes.
 */

public class Main {
  public static void main(String[] args) {
    ShapeMaker.getInstance()
        .drawCircle()
        .drawRectangle()
        .drawSquare();
  }
}
