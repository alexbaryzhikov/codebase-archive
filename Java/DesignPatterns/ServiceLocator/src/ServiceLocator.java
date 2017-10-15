public class ServiceLocator {

  private static Cache cache;
  private static InitialContext context;

  public static void setCache(Cache cache) {
    ServiceLocator.cache = cache;
  }

  public static void setContext(InitialContext context) {
    ServiceLocator.context = context;
  }

  public static Service getService(String serviceName) throws Exception {
    if (cache == null) {
      throw new Exception("Cache is not initialized");
    }

    Service service = cache.getService(serviceName);
    if (service != null) {
      return service;
    }

    if (context == null) {
      throw new Exception("Context is not initialized");
    }

    service = (Service) context.lookup(serviceName);
    cache.addService(service);
    return service;
  }
}
