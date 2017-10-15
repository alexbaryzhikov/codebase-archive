/* --------------------------------------------------------
 * Strategy
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Define a family of algorithms, encapsulate each one, and make them interchangeable. Strategy
 *   lets the algorithm vary independently from clients that use it
 *
 * Related patterns
 *
 * - Bridge - both of the patterns have the same UML diagram. But they differ in their intent since
 *   the strategy is related with the behavior and bridge is for structure
 */

public class Main {
  public static void main(String[] args) {
    Robot r1 = new Robot("BigRobot");
    Robot r2 = new Robot("George");
    Robot r3 = new Robot("R2");

    r1.setStrategy(new AggressiveStrategy());
    r2.setStrategy(new DefensiveStrategy());
    r3.setStrategy(new NormalStrategy());

    r1.move();
    r2.move();
    r3.move();

    System.out.println("\nNew strategies:");
    System.out.println(r1.getName() + " gets scared");
    System.out.println(r2.getName() + " gets mad");
    System.out.println(r3.getName() + " keeps calm\n");

    r1.setStrategy(new DefensiveStrategy());
    r2.setStrategy(new AggressiveStrategy());

    r1.move();
    r2.move();
    r3.move();
  }
}
