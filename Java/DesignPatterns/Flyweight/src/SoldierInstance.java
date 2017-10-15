/**
 * "Heavyweight" unit object, which is the client of flyweight unit. This object
 * provides all unit services and is used in the game
 */
public class SoldierInstance {

  /**
   * Reference to flyweight
   */
  private Unit soldier = UnitFactory.getUnit(Unit.SOLDIER);

  private int locationX = 0;
  private int locationY = 0;

  public void move(int newLocationX, int newLocationY) {
    // Rendering is handled by the flyweight object
    soldier.move(locationX, locationY, newLocationX, newLocationY);

    // Maintaining the state extrinsic to the flyweight
    locationX = newLocationX;
    locationY = newLocationY;
  }
}
