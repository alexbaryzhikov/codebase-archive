import java.util.HashMap;

public class UnitFactory {

  /**
   * Units pool
   */
  private static final HashMap<String, Unit> units = new HashMap<>();

  public static Unit getUnit(String unitType) {
    Unit unit = units.get(unitType);
    if (unit == null) {
      switch (unitType) {
        case Unit.SOLDIER:
          unit = new Soldier();
          units.put(Unit.SOLDIER, unit);
          System.out.println("Creating a soldier");
          break;
        case Unit.TANK:
          unit = new Tank();
          units.put(Unit.TANK, unit);
          System.out.println("Creating a tank");
          break;
        default:
          throw new IllegalArgumentException("Unknown unit type: " + unitType);
      }
    }
    return unit;
  }
}
