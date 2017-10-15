/* --------------------------------------------------------
 * Abstract Factory
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Offer the interface for creating a family of related objects, without explicitly specifying their classes
 *
 * Applicability
 *
 * - System needs to be independent from the way the products it works with are created
 * - System is configured to work with multiple families of products
 * - A family of products is designed to work only all together
 * - Creation of a library of products for which only the interface is relevant, not the implementation
 *
 * Examples
 *
 * - Phone numbers and addresses factory for different countries/regions
 * - Pizza factory for different pizza shops
 * - GUI theme factory
 */

public class Main {
  public static void main(String[] args) {
    // Malevich's factory
    AbstractFactory malevichFactory = FactoryBuilder.getFactory(AbstractFactory.MALEVICH);

    malevichFactory.getShape().draw();
    malevichFactory.getColor().fill();

    // Kandinsky's factory
    AbstractFactory kandinskyFactory = FactoryBuilder.getFactory(AbstractFactory.KANDINSKY);

    kandinskyFactory.getShape().draw();
    kandinskyFactory.getColor().fill();
  }
}
