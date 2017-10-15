/**
 * Flyweight unit object
 */
public class Soldier implements Unit {

  /**
   * Intrinsic state maintained by flyweight implementation
   */
  private Object soldierVisualRepresentation;

  /**
   * Unit location is extrinsic, no reference to location is maintained inside flyweight
   */
  @Override
  public void move(int prevX, int prevY, int newX, int newY) {
    System.out.println("Soldier::move to " + newX + ", " + newY);
  }
}
