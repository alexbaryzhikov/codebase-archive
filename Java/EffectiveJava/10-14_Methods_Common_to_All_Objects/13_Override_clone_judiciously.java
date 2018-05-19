// Expressions must be true
x.clone() != x  // required
x.clone().getClass() == x.getClass()  // optional
x.clone().equals(x)  // optional

// NOTE If you override the clone method in a nonfinal class, you should return an object obtained
// by invoking `super.clone`.

// NOTE In practice, a class that implements Cloneable is expected to provide a properly
// functioning public clone method.

@Override public PhoneNumber clone() {
  try {
    return (PhoneNumber) super.clone();
  } catch(CloneNotSupportedException e) {
    throw new AssertionError();  // Can't happen
  }
}

// NOTE Never make the client do anything the library can do for the client.

// -------------------------------------------------------------------------------------------------

public class Stack {

  private static final int DEFAULT_INITIAL_CAPACITY = 16;

  private Object[] elements;
  private int size = 0;

  public Stack() {
    this.elements = new Object[DEFAULT_INITIAL_CAPACITY];
  }

  public void push(Object e) {
    ensureCapacity();
    elements[size++] = e;
  }

  public Object pop() {
    if (size == 0) {
      throw new EmptyStackException();
    }
    Object result = elements[--size];
    elements[size] = null;  // Eliminate obsolete reference
    return result;
  }

  // Ensure space for at least one more element.
  private void ensureCapacity() {
    if (elements.length == size) {
      elements = Arrays.copyOf(elements, 2 * size + 1);
    }
  }
}

// NOTE In effect, the clone method functions as another constructor; you must ensure that it does
// no harm to the original object and that it properly establishes invariants on the clone.

@Override public Stack clone() {
  try {
    Stack result = (Stack) super.clone();
    result.elements = elements.clone();
    return result;
  } catch (CloneNotSupportedException e) {
    throw new AssertionError();
  }
}

// -------------------------------------------------------------------------------------------------

// NOTE The clone architecture is incompatible with normal use of final fields referring to mutable
// objects.

public class HashTable implements Cloneable {

  private Entry[] buckets = ...;

  private static class Entry {

    final Object key;
    Object value;
    Entry next;

    Entry(Object key, Object value, Entry next) {
      this.key = key;
      this.value = value;
      this.next = next;
    }
  }

  ...  // Remainder omitted
}

// Broken - results in shared internal state!
@Override public HashTable clone() {
  try {
    HashTable result = (HashTable) super.clone();
    result.buckets = buckets.clone();
    return result;
  } catch (CloneNotSupportedException e) {
    throw new AssertionError();
  }
}


public class HashTable implements Cloneable {

  private Entry[] buckets = ...;

  private static class Entry {

    final Object key;
    Object value;
    Entry next;

    Entry(Object key, Object value, Entry next) {
      this.key = key;
      this.value = value;
      this.next = next;
    }

    // Recursively copy the linked list headed by this Entry
    Entry deepCopy() {
      return new Entry(key, value, next == null ? null : next.deepCopy());
    }
  }

  @Override public HashTable clone() {
    try {
      HashTable result = (HashTable) super.clone();
      result.buckets = new Entry[buckets.length];
      for (int i = 0; i < buckets.length; i++) {
        if (buckets[i] != null) {
          result.buckets[i] = buckets[i].deepCopy();
        }
      }
      return result;
    } catch (CloneNotSupportedException e) {
      throw new AssertionError();
    }
  }

  ...  // Remainder omitted
}


// Iteratively copy the linked list headed by this Entry
Entry deepCopy() {
  Entry result = new Entry(key, value, next);
  for (Entry p = result; p.next != null; p = p.next) {
    p.next = new Entry(p.next.key, p.next.value, p.next.next);
  }
  return result;
}

// -------------------------------------------------------------------------------------------------

// NOTE If you extend a class that implements Cloneable, you have little choice but to implement a
// well-behaved clone method. Otherwise, you are better off providing an alternative means of
// object copying, or simply not providing the capability.

// NOTE A fine approach to object copying is to provide a copy constructor or copy factory.

public Yum(Yum yum);

public static Yum newInstance(Yum yum);
