public class Circle implements Shape {

  private String name;

  // Register class with factories
  static {
    FactoryRegistrationWithReflection.getInstance().registerShape(Shape.CIRCLE, Circle.class);
    FactoryRegistration.getInstance().registerShape(Shape.CIRCLE, new Circle("circle_object"));
  }

  Circle(String name) {
    this.name = name;
    System.out.println("Creating circle: " + name);
  }

  @Override
  public Shape create(String name) {
    return new Circle(name);
  }

  @Override
  public void draw() {
    System.out.println("Drawing " + name);
  }
}
