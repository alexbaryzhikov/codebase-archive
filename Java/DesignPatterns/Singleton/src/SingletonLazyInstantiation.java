/**
 * Lazy instantiation using double locking mechanism. No need to sync if object exists
 */
public class SingletonLazyInstantiation {
  private static SingletonLazyInstantiation instance;

  public static SingletonLazyInstantiation getInstance() {
    if (instance == null) {
      synchronized (SingletonLazyInstantiation.class) {
        if (instance == null) {
          instance = new SingletonLazyInstantiation();
        }
      }
    }
    return instance;
  }

  private SingletonLazyInstantiation() {
  }

  public void showMessage() {
    System.out.println("SingletonLazyInstantiation::showMessage");
  }
}
