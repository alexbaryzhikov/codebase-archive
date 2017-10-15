/*
 * --------------------------------------------------------
 * Factory
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Creates objects without exposing the instantiation logic to the client
 * - Refers to the newly created object through a common interface
 */

public class Main {

  // Initialize shape classes
  static {
    try {
      Class.forName("Circle");
      Class.forName("Rectangle");
      Class.forName("Square");
    } catch (ClassNotFoundException e) {
      System.err.println("ClassNotFoundException" + e.getMessage());
    }
  }

  public static void main(String[] args) {
    // Procedural
    FactoryProcedural factory01 = FactoryProcedural.getInstance();
    factory01.createShape(Shape.CIRCLE, "circle01").draw();
    factory01.createShape(Shape.RECTANGLE, "rectangle01").draw();
    factory01.createShape(Shape.SQUARE, "square01").draw();

    // Class registration - using reflection
    FactoryRegistrationWithReflection factory02 = FactoryRegistrationWithReflection.getInstance();
    factory02.createShape(Shape.CIRCLE, "circle02").draw();
    factory02.createShape(Shape.RECTANGLE, "rectangle02").draw();
    factory02.createShape(Shape.SQUARE, "square02").draw();

    // Class registration - avoiding reflection
    FactoryRegistration factory03 = FactoryRegistration.getInstance();
    factory03.createShape(Shape.CIRCLE, "circle03").draw();
    factory03.createShape(Shape.RECTANGLE, "rectangle03").draw();
    factory03.createShape(Shape.SQUARE, "square03").draw();
  }
}
