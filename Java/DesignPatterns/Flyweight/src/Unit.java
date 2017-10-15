/**
 * Flyweight interface
 */
public interface Unit {

  String SOLDIER = "soldier";
  String TANK = "tank";

  /**
   * Move unit from previous location to new location. Unit location is extrinsic
   */
  void move(int prevX, int prevY, int newX, int newY);
}
