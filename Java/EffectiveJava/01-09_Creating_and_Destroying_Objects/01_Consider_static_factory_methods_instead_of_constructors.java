
// Static factory method

public static Boolean valueOf(boolean b) {
  return b ? Boolean.TRUE : Boolean.FALSE;
}

// NOTE One advantage of static factory methods is that, unlike constructors, they have names.

// NOTE A second advantage of static factory methods is that, unlike constructors, they are not
// required to create a new object each time they’re invoked.

// NOTE A third advantage of static factory methods is that, unlike constructors, they can return
// an object of any subtype of their return type.

// NOTE A fourth advantage of static factories is that the class of the returned object can vary
// from call to call as a function of the input parameters.

// NOTE A fifth advantage of static factories is that the class of the returned object need not
// exist when the class containing the method is written.

// -------------------------------------------------------------------------------------------------

// Service provider framework sketch

// Service interface
public interface Service {
  ... // Service-specific methods go here
}

// Service provider interface
public interface Provider {
  Service newService();
}

// Noninstantiable class for service registration and access
public class Services {

  private Services() { } // Prevents instantiation (Item 4)

  // Maps service names to services
  private static final Map<String, Provider> providers = new ConcurrentHashMap<String, Provider>();
  public static final String DEFAULT_PROVIDER_NAME = "<def>";

  // Provider registration API
  public static void registerDefaultProvider(Provider p) {
    registerProvider(DEFAULT_PROVIDER_NAME, p);
  }

  public static void registerProvider(String name, Provider p){
    providers.put(name, p);
  }

  // Service access API
  public static Service newInstance() {
    return newInstance(DEFAULT_PROVIDER_NAME);
  }

  public static Service newInstance(String name) {
    Provider p = providers.get(name);
    if (p == null)
    throw new IllegalArgumentException("No provider registered with name: " + name);
    return p.newService();
  }
}

// -------------------------------------------------------------------------------------------------

// NOTE The main disadvantage of providing only static factory methods is that classes without
// public or protected constructors cannot be subclassed.

// NOTE A second disadvantage of static factory methods is that they are not readily
// distinguishable from other static methods.
