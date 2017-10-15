/* --------------------------------------------------------
 * Bridge
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Decouple abstraction from implementation so that the two can vary independently
 *
 * We consider the case of a persistence API can have many implementations depending
 * on the presence or absence of a relational database, a file system, as well as
 * on the underlying operating system.
 */

public class Main {
  public static void main(String[] args) {
    PersistenceProvider provider = null;
    if (databaseDriverExists()) {
      provider = new DatabasePersistenceProvider();
    } else {
      provider = new FileSystemPersistenceProvider();
    }
    Persistence keeper = new Keeper(provider);

    // Get object
    Object o = keeper.findById("1234565");

    // Modifying object...

    // Save object
    keeper.persist(o);

    // Change provider
    keeper = new Keeper(new DatabasePersistenceProvider());
    keeper.deleteById("23232");
  }

  private static boolean databaseDriverExists() {
    return false;
  }
}
