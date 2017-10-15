/**
 * Flyweight unit object
 */
public class Tank implements Unit {

  /**
   * Intrinsic state maintained by flyweight implementation
   */
  private Object tankVisualRepresentation;

  /**
   * Unit location is extrinsic, no reference to location is maintained inside flyweight
   */
  @Override
  public void move(int prevX, int prevY, int newX, int newY) {
    System.out.println("Tank::move to " + newX + ", " + newY);
  }
}
