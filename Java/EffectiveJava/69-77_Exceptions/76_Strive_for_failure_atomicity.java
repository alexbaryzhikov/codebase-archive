// NOTE Generally speaking, a failed method invocation should leave the object in the state that it
// was in prior to the invocation.


public Object pop() {
  if (size == 0) {
    throw new EmptyStackException();
  }
  Object result = elements[--size];
  elements[size] = null; // Eliminate obsolete reference
  return result;
}
