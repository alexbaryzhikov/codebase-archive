// Returns maximum value in collection - throws exception if empty
public static <E extends Comparable<E>> E max(Collection<E> c) {
  if (c.isEmpty()) {
    throw new IllegalArgumentException("Empty collection");
  }
  E result = null;
  for (E e : c) {
    if (result == null || e.compareTo(result) > 0) {
      result = Objects.requireNonNull(e);
    }
  }
  return result;
}


// Returns maximum value in collection as an Optional<E>
public static <E extends Comparable<E>> Optional<E> max(Collection<E> c) {
  if (c.isEmpty()) {
    return Optional.empty();
  }
  E result = null;
  for (E e : c) {
    if (result == null || e.compareTo(result) > 0) {
      result = Objects.requireNonNull(e);
    }
  }
  return Optional.of(result);
}


// NOTE Never return a null value from an Optional-returning method.


// Returns max val in collection as Optional<E> - uses stream
public static <E extends Comparable<E>> Optional<E> max(Collection<E> c) {
  return c.stream().max(Comparator.naturalOrder());
}


// NOTE Optionals are similar in spirit to checked exceptions.


// Using an optional to provide a chosen default value
String lastWordInLexicon = max(words).orElse("No words...");


// Using an optional to throw a chosen exception
Toy myToy = max(toys).orElseThrow(TemperTantrumException::new);


// Using optional when you know there’s a return value
Element lastNobleGas = max(Elements.NOBLE_GASES).get();


Optional<ProcessHandle> parentProcess = ph.parent();
System.out.println("Parent PID: " + (parentProcess.isPresent() ?
  String.valueOf(parentProcess.get().pid()) : "N/A"));


System.out.println("Parent PID: " +
  ph.parent().map(h -> String.valueOf(h.pid())).orElse("N/A"));


// Stream<Optional<T>> to Stream<T> in Java 8
streamOfOptionals
  .filter(Optional::isPresent)
  .map(Optional::get)

// In Java 9
streamOfOptionals.
  .flatMap(Optional::stream)


// NOTE Container types, including collections, maps, streams, arrays, and optionals should not be
// wrapped in optionals.

// NOTE You should declare a method to return Optional<T> if it might not be able to return a
// result and clients will have to perform special processing if no result is returned.

// NOTE You should never return an optional of a boxed primitive type.

// NOTE It is almost never appropriate to use an optional as a key, value, or element in a
// collection or array.
