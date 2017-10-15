/* --------------------------------------------------------
 * Iterator
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Access contents of a collection without exposing its internal structure
 * - Support multiple simultaneous traversals of a collection
 * - Provide a uniform interface for traversing different collection
 */

public class Main {
  public static void main(String[] args) {
    NameRepository nameRepository = new NameRepository();
    for (Iterator it = nameRepository.getIterator(); it.hasNext(); ) {
      System.out.println("Name: " + it.next());
    }
  }
}
