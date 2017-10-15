/**
 * Provides file system storage
 */
public class FileSystemPersistenceProvider implements PersistenceProvider {
  @Override
  public long saveObject(Object object) {
    long objectId = System.currentTimeMillis();
    System.out.println("Writing object " + objectId + " to file");
    return objectId;
  }

  @Override
  public Object getObject(long objectId) {
    System.out.println("Reading object " + objectId + " from file");
    return null;
  }

  @Override
  public void deleteObject(long objectId) {
    System.out.println("Deleting object " + objectId + " file");
  }
}
