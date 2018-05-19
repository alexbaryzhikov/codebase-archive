public class Elvis {
  public static final Elvis INSTANCE = new Elvis();
  private Elvis() { ... }

  public void leaveTheBuilding() { ... }
}


// readResolve for instance control - you can do better!
private Object readResolve() {
  // Return the one true Elvis and let the garbage collector
  // take care of the Elvis impersonator.
  return INSTANCE;
}


// If you depend on readResolve for instance control, all instance fields with object reference
// types must be declared transient.


// Broken singleton - has nontransient object reference field!
public class Elvis implements Serializable {
  public static final Elvis INSTANCE = new Elvis();

  private Elvis() { }

  private String[] favoriteSongs = { "Hound Dog", "Heartbreak Hotel" };

  public void printFavorites() {
    System.out.println(Arrays.toString(favoriteSongs));
  }

  private Object readResolve() throws ObjectStreamException {
    return INSTANCE;
  }
}


// Here is a “stealer” class, constructed as per the description above:
public class ElvisStealer implements Serializable {
  static Elvis impersonator;
  private Elvis payload;

  private Object readResolve() {
    // Save a reference to the "unresolved" Elvis instance
    impersonator = payload;
    // Return an object of correct type for favorites field
    return new String[] { "A Fool Such as I" };
  }

  private static final long serialVersionUID = 0;
}

// This ugly program that deserializes a handcrafted stream to produce two distinct instances of
// the flawed singleton
public class ElvisImpersonator {
  // Byte stream could not have come from real Elvis instance!
  private static final byte[] serializedForm = new byte[] {
    (byte)0xac, (byte)0xed, 0x00, 0x05, 0x73, 0x72, 0x00, 0x05,
    0x45, 0x6c, 0x76, 0x69, 0x73, (byte)0x84, (byte)0xe6,
    (byte)0x93, 0x33, (byte)0xc3, (byte)0xf4, (byte)0x8b,
    0x32, 0x02, 0x00, 0x01, 0x4c, 0x00, 0x0d, 0x66, 0x61, 0x76,
    0x6f, 0x72, 0x69, 0x74, 0x65, 0x53, 0x6f, 0x6e, 0x67, 0x73,
    0x74, 0x00, 0x12, 0x4c, 0x6a, 0x61, 0x76, 0x61, 0x2f, 0x6c,
    0x61, 0x6e, 0x67, 0x2f, 0x4f, 0x62, 0x6a, 0x65, 0x63, 0x74,
    0x3b, 0x78, 0x70, 0x73, 0x72, 0x00, 0x0c, 0x45, 0x6c, 0x76,
    0x69, 0x73, 0x53, 0x74, 0x65, 0x61, 0x6c, 0x65, 0x72, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x02, 0x00, 0x01,
    0x4c, 0x00, 0x07, 0x70, 0x61, 0x79, 0x6c, 0x6f, 0x61, 0x64,
    0x74, 0x00, 0x07, 0x4c, 0x45, 0x6c, 0x76, 0x69, 0x73, 0x3b,
    0x78, 0x70, 0x71, 0x00, 0x7e, 0x00, 0x02
  };

  public static void main(String[] args) {
    // Initializes ElvisStealer.impersonator and returns
    // the real Elvis (which is Elvis.INSTANCE)
    Elvis elvis = (Elvis) deserialize(serializedForm);
    Elvis impersonator = ElvisStealer.impersonator;

    elvis.printFavorites();
    impersonator.printFavorites();
  }
}


// Enum singleton - the preferred approach
public enum Elvis {
  INSTANCE;

  private String[] favoriteSongs = { "Hound Dog", "Heartbreak Hotel" };

  public void printFavorites() {
    System.out.println(Arrays.toString(favoriteSongs));
  }
}


// NOTE The accessibility of readResolve is significant.
