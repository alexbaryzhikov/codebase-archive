// NOTE Strings are poor substitutes for other value types.

// NOTE Strings are poor substitutes for enum types.

// NOTE Strings are poor substitutes for aggregate types.


// Inappropriate use of string as aggregate type
String compoundKey = className + "#" + i.next();


// NOTE Strings are poor substitutes for capabilities.


// Broken - inappropriate use of string as capability!
public class ThreadLocal {
  private ThreadLocal() { } // Noninstantiable

  // Sets the current thread's value for the named variable.
  public static void set(String key, Object value);

  // Returns the current thread's value for the named variable.
  public static Object get(String key);
}


// This API can be fixed by replacing the string with an unforgeable key (sometimes called
// a capability)
public class ThreadLocal {
  private ThreadLocal() { }  // Noninstantiable

  public static class Key {  // (Capability)
    Key() { }
  }

    // Generates a unique, unforgeable key
  public static Key getKey() {
    return new Key();
  }

  public static void set(Key key, Object value);
  public static Object get(Key key);
}


// Do not need static, not typesafe
public final class ThreadLocal {
  public ThreadLocal() { }

  public void set(Object value);
  public Object get();
}


// Typesafe
public final class ThreadLocal<T> {
  public ThreadLocal() { }

  public void set(T value);
  public T get();
}
