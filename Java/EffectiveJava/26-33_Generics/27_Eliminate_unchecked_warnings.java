Set<Lark> exaltation = new HashSet();

// Venery.java:4: warning: [unchecked] unchecked conversion found: HashSet, required: Set<Lark>
//     Set<Lark> exaltation = new HashSet();
//                          ^

Set<Lark> exaltation = new HashSet<>();


// NOTE Eliminate every unchecked warning that you can.

// NOTE If you can’t eliminate a warning, and you can prove that the code that provoked the warning
// is typesafe, then (and only then) suppress the warning with an @SuppressWarnings("unchecked")
// annotation.

// NOTE Always use the SuppressWarnings annotation on the smallest scope possible.


public <T> T[] toArray(T[] a) {
  if (a.length < size) {
    return (T[]) Arrays.copyOf(elements, size, a.getClass());
  }
  System.arraycopy(elements, 0, a, 0, size);
  if (a.length > size) {
    a[size] = null;
  }
  return a;
}

// ArrayList.java:305: warning: [unchecked] unchecked cast found: Object[], required: T[]
//     return (T[]) Arrays.copyOf(elements, size, a.getClass());
//                               ^

// Adding local variable to reduce scope of @SuppressWarnings
public <T> T[] toArray(T[] a) {
  if (a.length < size) {
    // This cast is correct because the array we're creating
    // is of the same type as the one passed in, which is T[].
    @SuppressWarnings("unchecked")
    T[] result = (T[]) Arrays.copyOf(elements, size, a.getClass());
    return result;
  }
  System.arraycopy(elements, 0, a, 0, size);
  if (a.length > size) {
    a[size] = null;
  }
  return a;
}


// NOTE Every time you use an @SuppressWarnings("unchecked") annotation, add a comment saying why
// it’s safe to do so.
