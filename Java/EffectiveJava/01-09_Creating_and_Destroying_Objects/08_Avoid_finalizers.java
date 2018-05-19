
// NOTE Finalizers are unpredictable, often dangerous, and generally unnecessary.

// NOTE Never do anything time-critical in a finalizer.

// NOTE Never depend on a finalizer to update critical persistent state.

// NOTE There is a severe performance penalty for using finalizers.

// NOTE Provide an explicit termination method.

// NOTE Explicit termination methods are typically used in combination with the try-finally
// construct to ensure termination.

// try-finally block guarantees execution of termination method
Foo foo = new Foo(...);
try {
  // Do what must be done with foo
  ...
} finally {
  foo.terminate(); // Explicit termination method
}

// NOTE The finalizer should log a warning if it finds that the resource has not been terminated.


// Manual finalizer chaining
@Override protected void finalize() throws Throwable {
  try {
    ... // Finalize subclass state
  } finally {
    super.finalize();
  }
}


// Finalizer Guardian idiom
public class Foo {
  // Sole purpose of this object is to finalize outer Foo object
  private final Object finalizerGuardian = new Object() {
    @Override protected void finalize() throws Throwable {
      ... // Finalize outer Foo object
    }
  };
  ... // Remainder omitted
}
