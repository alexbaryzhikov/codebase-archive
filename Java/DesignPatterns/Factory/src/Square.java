public class Square implements Shape {

  private String name;

  // Register class with factories
  static {
    FactoryRegistrationWithReflection.getInstance().registerShape(Shape.SQUARE, Square.class);
    FactoryRegistration.getInstance().registerShape(Shape.SQUARE, new Square("square_object"));
  }

  Square(String name) {
    this.name = name;
    System.out.println("Creating square: " + name);
  }

  @Override
  public Square create(String name) {
    return new Square(name);
  }

  @Override
  public void draw() {
    System.out.println("Drawing " + name);
  }
}
