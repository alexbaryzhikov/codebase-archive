public interface PersistenceProvider {
  long saveObject(Object object);

  Object getObject(long objectId);

  void deleteObject(long objectId);
}
