import java.util.HashMap;

public class FactoryRegistration {

  private static FactoryRegistration instance = new FactoryRegistration();

  private HashMap<Integer, Shape> registeredShapes = new HashMap<>();

  /**
   * Get factory instance
   */
  public static FactoryRegistration getInstance() {
    return instance;
  }

  /**
   * Register class with factory
   */
  public void registerShape(int shapeId, Shape shape) {
    registeredShapes.put(shapeId, shape);
  }

  /**
   * Create a new shape object
   */
  public Shape createShape(int shapeId, String name) {
    return registeredShapes.get(shapeId).create(name);
  }

  /**
   * Private factory constructor
   */
  private FactoryRegistration() {
  }
}
