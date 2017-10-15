import shapes.Circle;
import colors.Color;
import colors.Yellow;
import shapes.Shape;

public class KandinskyFactory extends AbstractFactory {

  @Override
  Shape getShape() {
    return new Circle();
  }

  @Override
  Color getColor() {
    return new Yellow();
  }
}
