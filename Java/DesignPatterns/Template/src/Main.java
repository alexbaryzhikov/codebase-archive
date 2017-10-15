/* --------------------------------------------------------
 * Template
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Define the skeleton of an algorithm in an operation, deferring some steps to subclasses
 * - Template Method lets subclasses redefine certain steps of an algorithm without letting them
 *   to change the algorithm's structure
 *
 * Similar patterns
 *
 * - Strategy uses delegation while Template uses inheritance
 */

public class Main {
  public static void main(String[] args) {
    new Cricket().play();
    new Football().play();
  }
}
