/* --------------------------------------------------------
 * Singleton
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Ensure that only one instance of a class is created
 * - Provide a global point of access to the object
 *
 * Applicability
 *
 * - Logger classes
 * - Configuration classes
 * - Accessing resources in shared mode
 * - Factories implemented as Singletons
 */

import java.io.*;

public class Main {
  public static void main(String[] args) {
    SingletonEarlyInstantiation.getInstance().showMessage();
    SingletonLazyInstantiation.getInstance().showMessage();
    try {
      // Serialize singleton object
      byte[] singletonBytes = objectToBytes(SingletonSerializable.getInstance());
      // Deserialize singleton object
      SingletonSerializable singleton = (SingletonSerializable) bytesToObject(singletonBytes);
      singleton.showMessage();
    } catch (IOException e) {
      System.err.println("IOException: " + e.getMessage());
    } catch (ClassNotFoundException e) {
      System.err.println("ClassNotFoundException: " + e.getMessage());
    }
  }

  /**
   * Convert object to byte array
   */
  private static byte[] objectToBytes(Object object) throws IOException {
    try (ByteArrayOutputStream bos = new ByteArrayOutputStream();
         ObjectOutput out = new ObjectOutputStream(bos)) {
      out.writeObject(object);
      return bos.toByteArray();
    }
  }

  /**
   * Convert byte array to object
   */
  private static Object bytesToObject(byte[] bytes) throws IOException, ClassNotFoundException {
    try (ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
         ObjectInput in = new ObjectInputStream(bis)) {
      return in.readObject();
    }
  }
}
