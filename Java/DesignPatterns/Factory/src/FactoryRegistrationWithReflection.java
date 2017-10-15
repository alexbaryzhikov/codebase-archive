import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;

public class FactoryRegistrationWithReflection {

  private static FactoryRegistrationWithReflection instance = new FactoryRegistrationWithReflection();

  private HashMap<Integer, Class<?>> registeredShapes = new HashMap<>();

  /**
   * Get factory instance
   */
  public static FactoryRegistrationWithReflection getInstance() {
    return instance;
  }

  /**
   * Register class with factory
   */
  public void registerShape(int shapeId, Class<?> shapeClass) {
    registeredShapes.put(shapeId, shapeClass);
  }

  /**
   * Create a new shape object
   */
  public Shape createShape(int shapeId, String name) {
    Class<?> shapeClass = registeredShapes.get(shapeId);
    Constructor<?> shapeConstructor;
    Shape newShape = null;
    try {
      shapeConstructor = shapeClass.getDeclaredConstructor(String.class);
      newShape = (Shape) shapeConstructor.newInstance(new Object[]{name});
    } catch (NoSuchMethodException e) {
      System.err.println("NoSuchMethodException: " + e.getMessage());
    } catch (InstantiationException e) {
      System.err.println("InstantiationException: " + e.getMessage());
    } catch (IllegalAccessException e) {
      System.err.println("IllegalAccessException: " + e.getMessage());
    } catch (InvocationTargetException e) {
      System.err.println("InvocationTargetException: " + e.getMessage());
    }
    if (newShape == null) {
      System.err.println("Failed to create a shape: " + shapeId);
    }
    return newShape;
  }

  /**
   * Private factory constructor
   */
  private FactoryRegistrationWithReflection() {
  }
}
