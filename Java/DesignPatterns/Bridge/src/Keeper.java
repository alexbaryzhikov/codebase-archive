/**
 * Keeper is used by client to manage persistence
 */
public class Keeper implements Persistence {

  private PersistenceProvider provider;

  Keeper(PersistenceProvider provider) {
    this.provider = provider;
  }

  @Override
  public String persist(Object object) {
    return Long.toString(provider.saveObject(object));
  }

  @Override
  public Object findById(String objectId) {
    return provider.getObject(Long.parseLong(objectId));
  }

  @Override
  public void deleteById(String objectId) {
    provider.deleteObject(Long.parseLong(objectId));
  }
}
