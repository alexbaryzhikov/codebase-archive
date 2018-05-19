
// Can you spot the "memory leak"?

public class Stack {

  private static final int DEFAULT_INITIAL_CAPACITY = 16;
  private Object[] elements;
  private int size = 0;

  public Stack() {
    elements = new Object[DEFAULT_INITIAL_CAPACITY];
  }

  public void push(Object e) {
    ensureCapacity();
    elements[size++] = e;
  }

  public Object pop() {
    if (size == 0) {
      throw new EmptyStackException();
    }
    return elements[--size];
  }

  /**
   * Ensure space for at least one more element, roughly
   * doubling the capacity each time the array needs to grow.
   */
  private void ensureCapacity() {
    if (elements.length == size) {
      elements = Arrays.copyOf(elements, 2 * size + 1);
    }
  }
}


// Corrected version of the pop method looks like this
public Object pop() {
  if (size == 0) {
    throw new EmptyStackException();
  }
  Object result = elements[--size];
  elements[size] = null; // Eliminate obsolete reference
  return result;
}

// NOTE Nulling out object references should be the exception rather than the norm.

// NOTE Whenever a class manages its own memory, the programmer should be alert for memory leaks.

// NOTE Another common source of memory leaks is caches.

// NOTE A third common source of memory leaks is listeners and other callbacks.
