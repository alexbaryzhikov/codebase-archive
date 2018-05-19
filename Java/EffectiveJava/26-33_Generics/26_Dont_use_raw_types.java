// Term                         Example
// --------------------------------------------------------------
// Parameterized type           List<String>
// Actual type parameter        String
// Generic type                 List<E>
// Formal type parameter        E
// Unbounded wildcard type      List<?>
// Raw type                     List
// Bounded type parameter       <E extends Number>
// Recursive type bound         <T extends Comparable<T>>
// Bounded wildcard type        List<? extends Number>
// Generic method               static <E> List<E> asList(E[] a)
// Type token                   String.class


// Now a raw collection type - don't do this!

/**
 * My stamp collection. Contains only Stamp instances.
 */
private final Collection stamps = ... ;


// Erroneous insertion of coin into stamp collection
stamps.add(new Coin( ... ));


// Now a raw iterator type - don't do this!
for (Iterator i = stamps.iterator(); i.hasNext(); ) {
  Stamp s = (Stamp) i.next(); // Throws ClassCastException
  ... // Do something with the stamp
}


// Parameterized collection type - typesafe
private final Collection<Stamp> stamps = ... ;


// for-each loop over a parameterized collection - typesafe
for (Stamp s : stamps) { // No cast
  ... // Do something with the stamp
}


// for loop with parameterized iterator declaration - typesafe
for (Iterator<Stamp> i = stamps.iterator(); i.hasNext(); ) {
  Stamp s = i.next(); // No cast necessary
  ... // Do something with the stamp
}


// NOTE If you use raw types, you lose all the safety and expressiveness benefits of generics.

// NOTE You lose type safety if you use a raw type like List, but not if you use a parameterized
// type like List<Object> .


// Uses raw type (List) - fails at runtime!
public static void main(String[] args) {
  List<String> strings = new ArrayList<>();
  unsafeAdd(strings, new Integer(42));
  String s = strings.get(0); // Compiler-generated cast
}

private static void unsafeAdd(List list, Object o) {
  list.add(o);
}


// Use of raw type for unknown element type - don't do this!
static int numElementsInCommon(Set s1, Set s2) {
  int result = 0;
  for (Object o1 : s1) {
    if (s2.contains(o1)) {
      result++;
    }
  }
  return result;
}


// Unbounded wildcard type - typesafe and flexible
static int numElementsInCommon(Set<?> s1, Set<?> s2) {
  int result = 0;
  for (Object o1 : s1) {
    if (s2.contains(o1)) {
      result++;
    }
  }
  return result;
}


// NOTE You can’t put any element (other than null ) into a Collection<?>.

// NOTE You must use raw types in class literals.

// NOTE This is the preferred way to use the instanceof operator with generic types:

// Legitimate use of raw type - instanceof operator
if (o instanceof Set) {
  // Raw type
  Set<?> m = (Set<?>) o;
  // Wildcard type
  ...
}
