public class Circle extends Shape {

  Circle() {
    type = "Circle";
  }

  @Override
  public void draw() {
    System.out.println("Inside Circle::draw() method");
  }
}
