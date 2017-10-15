/**
 * Facade class
 */
public class ShapeMaker {

  private static ShapeMaker instance = new ShapeMaker();

  private Shape circle;
  private Shape rectangle;
  private Shape square;

  private ShapeMaker() {
    circle = new Circle();
    rectangle = new Rectangle();
    square = new Square();
  }

  public static ShapeMaker getInstance() {
    return instance;
  }

  public ShapeMaker drawCircle() {
    circle.draw();
    return this;
  }

  public ShapeMaker drawRectangle() {
    rectangle.draw();
    return this;
  }

  public ShapeMaker drawSquare() {
    square.draw();
    return this;
  }
}
