/* --------------------------------------------------------
 * Observer
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Defines a one-to-many dependency between objects so that when one object changes state,
 *   all its dependents are notified and updated automatically
 */

public class Main {
  public static void main(String[] args) {
    Observable numberGenerator = new NumberGenerator();
    Observer binDisplay = new BinDisplay(numberGenerator);
    Observer octDisplay = new OctDisplay(numberGenerator);
    Observer hexDisplay = new HexDisplay(numberGenerator);

    System.out.println("Observable state changes to 15");
    numberGenerator.setState(15);

    // Detach binDisplay observer
    numberGenerator.detach(binDisplay);

    System.out.println("Observable state changes to 10");
    numberGenerator.setState(10);
  }
}
