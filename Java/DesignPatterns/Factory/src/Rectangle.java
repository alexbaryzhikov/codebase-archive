public class Rectangle implements Shape {

  private String name;

  // Register class with factories
  static {
    FactoryRegistrationWithReflection.getInstance().registerShape(Shape.RECTANGLE, Rectangle.class);
    FactoryRegistration.getInstance().registerShape(Shape.RECTANGLE, new Rectangle("rectangle_object"));
  }

  Rectangle(String name) {
    this.name = name;
    System.out.println("Creating rectangle: " + name);
  }

  @Override
  public Rectangle create(String name) {
    return new Rectangle(name);
  }

  @Override
  public void draw() {
    System.out.println("Drawing " + name);
  }
}
