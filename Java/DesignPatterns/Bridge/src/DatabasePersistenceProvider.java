public class DatabasePersistenceProvider implements PersistenceProvider {

  DatabasePersistenceProvider() {
    System.out.println("Loading database driver");
  }

  @Override
  public long saveObject(Object object) {
    long objectId = System.currentTimeMillis();
    System.out.println("Writing object " + objectId + " to database");
    return objectId;
  }

  @Override
  public Object getObject(long objectId) {
    System.out.println("Reading object " + objectId + " from database");
    return null;
  }

  @Override
  public void deleteObject(long objectId) {
    System.out.println("Deleting object " + objectId + " from database");
  }
}
