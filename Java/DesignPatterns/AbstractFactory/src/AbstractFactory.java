import colors.Color;
import shapes.Shape;

public abstract class AbstractFactory {

  // Factory type IDs
  public static final int MALEVICH = 0;
  public static final int KANDINSKY = 1;

  abstract Shape getShape();
  abstract Color getColor();
}
