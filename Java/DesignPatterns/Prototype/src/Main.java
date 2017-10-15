/* --------------------------------------------------------
 * Prototype
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Specifying the kind of objects to create using a prototypical instance
 * - Creating new objects by copying this prototype
 *
 * Applicability
 *
 * - Classes to be instantiated are specified at run-time
 * - Avoiding the creation of a factory hierarchy is needed
 * - It is more convenient to copy an existing instance than to create a new one
 */

public class Main {
  public static void main(String[] args) {
    ShapeCache.loadCache();

    Shape clonedShape = ShapeCache.getShape("1");
    System.out.println("Shape: " + clonedShape.getType());

    Shape clonedShape2 = ShapeCache.getShape("2");
    System.out.println("Shape: " + clonedShape2.getType());

    Shape clonedShape3 = ShapeCache.getShape("3");
    System.out.println("Shape: " + clonedShape3.getType());
  }
}
