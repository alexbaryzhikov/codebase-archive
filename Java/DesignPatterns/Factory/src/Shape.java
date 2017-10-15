public interface Shape {

  int CIRCLE = 0;
  int RECTANGLE = 1;
  int SQUARE = 2;

  Shape create(String name);
  void draw();
}
