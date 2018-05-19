// NOTE Do not accept the default serialized form without first considering whether it is
// appropriate.

// NOTE The default serialized form is likely to be appropriate if an object’s physical
// representation is identical to its logical content.

// Good candidate for default serialized form
public class Name implements Serializable {

  /**
  * Last name. Must be non-null.
  * @serial
  */
  private final String lastName;

  /**
  * First name. Must be non-null.
  * @serial
  */
  private final String firstName;

  /**
  * Middle name, or null if there is none.
  * @serial
  */
  private final String middleName;

  ... // Remainder omitted
}


// NOTE Even if you decide that the default serialized form is appropriate, you often must provide
// a readObject method to ensure invariants and security.


// Awful candidate for default serialized form
public final class StringList implements Serializable {
  private int size = 0;
  private Entry head = null;

  private static class Entry implements Serializable {
    String data;
    Entry next;
    Entry previous;
  }

  ... // Remainder omitted
}


// NOTE Using the default serialized form when an object’s physical representation differs
// substantially from its logical data content has four disadvantages:

// • It permanently ties the exported API to the current internal representation.
// • It can consume excessive space.
// • It can consume excessive time.
// • It can cause stack overflows.


// StringList with a reasonable custom serialized form
public final class StringList implements Serializable {
  private transient int size = 0;
  private transient Entry head = null;

  // No longer Serializable!
  private static class Entry {
    String data;
    Entry next;
    Entry previous;
  }

  // Appends the specified string to the list
  public final void add(String s) { ... }

  /**
   * Serialize this {@code StringList} instance.
   *
   * @serialData The size of the list (the number of strings
   * it contains) is emitted ({@code int}), followed by all of
   * its elements (each a {@code String}), in the proper
   * sequence.
   */
  private void writeObject(ObjectOutputStream s) throws IOException {
    s.defaultWriteObject();
    s.writeInt(size);

    // Write out all elements in the proper order.
    for (Entry e = head; e != null; e = e.next) {
      s.writeObject(e.data);
    }
  }

  private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
    s.defaultReadObject();
    int numElements = s.readInt();

    // Read in all elements and insert them in list
    for (int i = 0; i < numElements; i++) {
      add((String) s.readObject());
    }
  }

  ... // Remainder omitted
}


// NOTE If all instance fields are transient, it is technically permissible to dispense with
// invoking defaultWriteObject and defaultReadObject, but it is not recommended.

// NOTE Before deciding to make a field nontransient, convince yourself that its value is part of
// the logical state of the object.

// NOTE You must impose any synchronization on object serialization that you would impose on any
// other method that reads the entire state of the object.


// writeObject for synchronized class with default serialized form
private synchronized void writeObject(ObjectOutputStream s) throws IOException {
  s.defaultWriteObject();
}


// NOTE Regardless of what serialized form you choose, declare an explicit serial version UID in
// every serializable class you write.

private static final long serialVersionUID = randomLongValue ;
