/**
 * Early instantiation using implementation with static field. No need to sync
 */
public class SingletonEarlyInstantiation {
  private static SingletonEarlyInstantiation instance = new SingletonEarlyInstantiation();

  public static SingletonEarlyInstantiation getInstance() {
    return instance;
  }

  private SingletonEarlyInstantiation() {
  }

  public void showMessage() {
    System.out.println("SingletonEarlyInstantiation::showMessage");
  }
}
