// Serialization proxy for Period class
private static class SerializationProxy implements Serializable {
  private final Date start;
  private final Date end;

  SerializationProxy(Period p) {
    this.start = p.start;
    this.end = p.end;
  }

  private static final long serialVersionUID = 234098243823485285L; // Any number will do (Item 75)
}


// writeReplace method for the serialization proxy pattern
private Object writeReplace() {
  return new SerializationProxy(this);
}


// readObject method for the serialization proxy pattern
private void readObject(ObjectInputStream stream) throws InvalidObjectException {
  throw new InvalidObjectException("Proxy required");
}


// readResolve method for Period.SerializationProxy
private Object readResolve() {
  return new Period(start, end); // Uses public constructor
}


// EnumSet's serialization proxy
private static class SerializationProxy <E extends Enum<E>> implements Serializable {

  // The element type of this enum set.
  private final Class<E> elementType;

  // The elements contained in this enum set.
  private final Enum[] elements;

  SerializationProxy(EnumSet<E> set) {
    elementType = set.elementType;
    elements = set.toArray(EMPTY_ENUM_ARRAY); // (Item 43)
  }

  private Object readResolve() {
    EnumSet<E> result = EnumSet.noneOf(elementType);
    for (Enum e : elements) {
      result.add((E)e);
    }
    return result;
  }

  private static final long serialVersionUID = 362491234563181265L;
}
