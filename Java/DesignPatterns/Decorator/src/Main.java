/* --------------------------------------------------------
 * Decorator
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Add responsibilities to an object dynamically
 *
 * A decorator is different from an adapter in that a decorator changes object's responsibilities,
 * while an adapter changes an object interface.
 */

public class Main {
  public static void main(String[] args) {
    // Create a new window
    Window window = new SimpleWindow();
    window.renderWindow();

    // At some point window content becomes larger and scrolling behavior must be added
    System.out.println("Decorating window...");
    window = new ScrollableWindow(window);
    window.renderWindow();
  }
}
