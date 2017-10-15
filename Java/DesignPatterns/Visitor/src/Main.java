/* --------------------------------------------------------
 * Visitor
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Represents an operation to be performed on the elements of an object structure
 * - Enables defining a new operation without changing the classes of the elements on which
 *   it operates
 *
 * Applicability
 *
 * - Similar operations have to be performed on objects of different types grouped in a structure
 *   (a collection or a composite structure)
 * - There are many distinct and unrelated operations need to be performed. Visitor pattern allows
 *   to create a separate visitor concrete class for each type of operation and to separate this
 *   operation implementation from the objects structure
 * - The object structure is not likely to be changed but is very probable to have new operations
 *   which have to be added. Since the pattern separates the visitor (representing operations,
 *   algorithms, behaviors) from the object structure, it's very easy to add new visitors as long
 *   as the structure remains unchanged
 */

public class Main {
  public static void main(String[] args) {
    Computer computer = new Computer("Blazer", 4.7, 64.0, 5.0);
    Visitor inspector = new ComputerInspector();
    computer.accept(inspector);
  }
}
