
// NOTE Existing classes can be easily retrofitted to implement a new interface.

// NOTE Interfaces are ideal for defining mixins.

// NOTE Interfaces allow the construction of nonhierarchical type frameworks.

public interface Singer {
  AudioClip sing(Song s);
}

public interface Songwriter {
  Song compose(boolean hit);
}

public interface SingerSongwriter extends Singer, Songwriter {
  AudioClip strum();
  void actSensitive();
}

// NOTE Interfaces enable safe, powerful functionality enhancements.

// -------------------------------------------------------------------------------------------------

// NOTE You can combine the virtues of interfaces and abstract classes by providing an abstract
// skeletal implementation class to go with each nontrivial interface that you export.

// Concrete implementation built atop skeletal implementation
static List<Integer> intArrayAsList(final int[] a) {
  if (a == null) {
    throw new NullPointerException();
  }

  return new AbstractList<Integer>() {

    public Integer get(int i) {
      return a[i]; // Autoboxing (Item 5)
    }

    @Override public Integer set(int i, Integer val) {
      int oldVal = a[i];
      a[i] = val; // Auto-unboxing
      return oldVal; // Autoboxing
    }

    public int size() {
      return a.length;
    }
  };
}

// Skeletal Implementation
public abstract class AbstractMapEntry<K,V> implements Map.Entry<K,V> {

  // Primitive operations
  public abstract K getKey();
  public abstract V getValue();

  // Entries in modifiable maps must override this method
  public V setValue(V value) {
    throw new UnsupportedOperationException();
  }

  // Implements the general contract of Map.Entry.equals
  @Override public boolean equals(Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof Map.Entry)) {
      return false;
    }
    Map.Entry<?, ?> arg = (Map.Entry) o;
    return equals(getKey(), arg.getKey()) && equals(getValue(), arg.getValue());
  }

  private static boolean equals(Object o1, Object o2) {
    return o1 == null ? o2 == null : o1.equals(o2);
  }

  // Implements the general contract of Map.Entry.hashCode
  @Override public int hashCode() {
    return hashCode(getKey()) ^ hashCode(getValue());
  }

  private static int hashCode(Object obj) {
    return obj == null ? 0 : obj.hashCode();
  }
}

// -------------------------------------------------------------------------------------------------

// NOTE It is far easier to evolve an abstract class than an interface.

// NOTE Once an interface is released and widely implemented, it is almost impossible to change.
