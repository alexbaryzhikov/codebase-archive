import java.io.ObjectStreamException;
import java.io.Serializable;

public class SingletonSerializable implements Serializable {
  private static final long serialVersionUID = 42L;
  private static SingletonSerializable instance = new SingletonSerializable();

  public static SingletonSerializable getInstance() {
    return instance;
  }

  public void showMessage() {
    System.out.println("SingletonSerializable::showMessage");
  }

  protected Object readResolve() throws ObjectStreamException {
    return getInstance();
  }

  private SingletonSerializable() {
  }
}
