import java.util.HashMap;

public class ShapeCache {

  private static HashMap<String, Shape> shapeMap = new HashMap<>();

  public static Shape getShape(String shapeId) {
    Shape cachedShape = shapeMap.get(shapeId);
    return (Shape) cachedShape.clone();
  }

  /* For each shape run database query and create shape
   * shapeMap.put(shapeKey, shape);
   * For example, we are adding three shapes */

  public static void loadCache() {
    Shape circle = new Circle();
    circle.setId("1");
    shapeMap.put(circle.getId(), circle);

    Shape square = new Square();
    square.setId("2");
    shapeMap.put(square.getId(), square);

    Shape rectangle = new Rectangle();
    rectangle.setId("3");
    shapeMap.put(rectangle.getId(), rectangle);
  }
}
