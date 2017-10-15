public class FactoryProcedural {

  private static FactoryProcedural instance = new FactoryProcedural();

  /**
   * Get factory instance
   */
  public static FactoryProcedural getInstance() {
    return instance;
  }

  /**
   * Create a new shape object
   */
  public Shape createShape(int shapeType, String name) {
    switch (shapeType) {
      case Shape.CIRCLE:
        return new Circle(name);
      case Shape.RECTANGLE:
        return new Rectangle(name);
      case Shape.SQUARE:
        return new Square(name);
      default:
        return null;
    }
  }

  /**
   * Private factory constructor
   */
  private FactoryProcedural() {
  }
}
