import colors.Black;
import colors.Color;
import shapes.Shape;
import shapes.Square;

public class MalevichFactory extends AbstractFactory {

  @Override
  Shape getShape() {
    return new Square();
  }

  @Override
  Color getColor() {
    return new Black();
  }
}
