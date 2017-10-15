/* --------------------------------------------------------
 * Flyweight
 * --------------------------------------------------------
 *
 * Intent
 * - Use sharing to support a large number of objects that have part of their internal state
 *   in common where the other part of state can vary
 *
 * Examples
 *
 * - War game with combat units objects
 * - Text editor with characters objects
 */

import java.util.Random;

public class Main {
  public static void main(String[] args) {
    System.out.println("Starting war");
    System.out.println("Drawing terrain");
    System.out.println("Creating combat units");

    // Create soldiers
    SoldierInstance[] soldiers = {
        new SoldierInstance(),
        new SoldierInstance(),
        new SoldierInstance(),
        new SoldierInstance(),
        new SoldierInstance()
    };

    // Create tanks
    TankInstance[] tanks = {
        new TankInstance(),
        new TankInstance(),
        new TankInstance()
    };

    System.out.println("Moving units to their locations");

    Random random = new Random();
    for (SoldierInstance soldier : soldiers) {
      soldier.move(random.nextInt(1000), random.nextInt(1000));
    }
    for (TankInstance tank : tanks) {
      tank.move(random.nextInt(1000), random.nextInt(1000));
    }
  }
}
