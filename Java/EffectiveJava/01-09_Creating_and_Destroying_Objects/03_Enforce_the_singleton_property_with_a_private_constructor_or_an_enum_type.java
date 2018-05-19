
// NOTE Making a class a singleton can make it difficult to test its clients

// Singleton with public final field

public class Elvis {
  public static final Elvis INSTANCE = new Elvis();
  private Elvis() { ... }
  public void leaveTheBuilding() { ... }
}


// Singleton with static factory

public class Elvis {
  private static final Elvis INSTANCE = new Elvis();
  private Elvis() { ... }
  public static Elvis getInstance() { return INSTANCE; }
  public void leaveTheBuilding() { ... }
}


// readResolve method to preserve singleton property

private Object readResolve() {
  // Return the one true Elvis and let the garbage collector
  // take care of the Elvis impersonator.
  return INSTANCE;
}


// Enum singleton - the preferred approach

public enum Elvis {
  INSTANCE;
  public void leaveTheBuilding() { ... }
}

// NOTE A single-element enum type is the best way to implement a singleton.
