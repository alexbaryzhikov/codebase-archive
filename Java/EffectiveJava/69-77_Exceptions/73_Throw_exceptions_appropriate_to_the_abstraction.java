// NOTE Higher layers should catch lower-level exceptions and, in their place, throw exceptions
// that can be explained in terms of the higher-level abstraction.


// Exception Translation
try {
  // Use lower-level abstraction to do our bidding
  ...
} catch(LowerLevelException e) {
  throw new HigherLevelException(...);
}


/**
 * Returns the element at the specified position in this list.
 * @throws IndexOutOfBoundsException if the index is out of range
 *         ({@code index < 0 || index >= size()}).
 */
public E get(int index) {
  ListIterator<E> i = listIterator(index);
  try {
    return i.next();
  } catch(NoSuchElementException e) {
    throw new IndexOutOfBoundsException("Index: " + index);
  }
}


// Exception Chaining
try {
  ... // Use lower-level abstraction to do our bidding
} catch (LowerLevelException cause) {
  throw new HigherLevelException(cause);
}


// Exception with chaining-aware constructor
class HigherLevelException extends Exception {
  HigherLevelException(Throwable cause) {
    super(cause);
  }
}


// NOTE While exception translation is superior to mindless propagation of exceptions from lower
// layers, it should not be overused.
