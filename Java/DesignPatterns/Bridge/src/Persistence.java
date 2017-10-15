public interface Persistence {
  String persist(Object object);

  Object findById(String objectId);

  void deleteById(String objectId);
}
