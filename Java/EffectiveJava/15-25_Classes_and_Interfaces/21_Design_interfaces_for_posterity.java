// NOTE It is not always possible to write a default method that maintains all invariants of every
// conceivable implementation.


// Default method added to the Collection interface in Java 8
default boolean removeIf(Predicate<? super E> filter) {
  Objects.requireNonNull(filter);
  boolean result = false;
  for (Iterator<E> it = iterator(); it.hasNext(); ) {
    if (filter.test(it.next())) {
      it.remove();
      result = true;
    }
  }
  return result;
}


// NOTE In the presence of default methods, existing implementations of an interface may compile
// without error or warning but fail at runtime.

// NOTE It is still of the utmost importance to design interfaces with great care.

// NOTE While it may be possible to correct some interface flaws after an interface is released,
// you cannot count on it.
