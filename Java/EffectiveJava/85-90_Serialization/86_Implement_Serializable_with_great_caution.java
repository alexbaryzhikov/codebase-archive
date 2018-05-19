// NOTE A major cost of implementing Serializable is that it decreases the flexibility to change
// a class’s implementation once it has been released.

// NOTE A second cost of implementing Serializable is that it increases the likelihood of bugs and
// security holes.

// NOTE A third cost of implementing Serializable is that it increases the testing burden
// associated with releasing a new version of a class.

// NOTE Implementing the Serializable interface is not a decision to be undertaken lightly.

// NOTE Classes designed for inheritance (Item 17) should rarely implement Serializable, and
// interfaces should rarely extend it.


// If the class has invariants that would be violated if its instance fields were initialized to
// their default values, you must add this readObjectNoData method to the class:

// readObjectNoData for stateful extendable serializable classes
private void readObjectNoData() throws InvalidObjectException {
  throw new InvalidObjectException("Stream data required");
}


// NOTE You should consider providing a parameterless constructor on nonserializable classes
// designed for inheritance.


// Suppose the class has one constructor:
public AbstractFoo(int x, int y) { ... }


// Nonserializable stateful class allowing serializable subclass
public abstract class AbstractFoo {
  private int x, y; // Our state

  // This enum and field are used to track initialization
  private enum State { NEW, INITIALIZING, INITIALIZED };
  private final AtomicReference<State> init = new AtomicReference<>(State.NEW);

  public AbstractFoo(int x, int y) { initialize(x, y); }

  // This constructor and the following method allow
  // subclass's readObject method to initialize our state.
  protected AbstractFoo() { }

  protected final void initialize(int x, int y) {
    if (!init.compareAndSet(State.NEW, State.INITIALIZING)) {
      throw new IllegalStateException("Already initialized");
    }
    this.x = x;
    this.y = y;
    ... // Do anything else the original constructor did
    init.set(State.INITIALIZED);
  }

  // These methods provide access to internal state so it can
  // be manually serialized by subclass's writeObject method.
  protected final int getX() { checkInit(); return x; }
  protected final int getY() { checkInit(); return y; }

  // Must call from all public and protected instance methods
  private void checkInit() {
    if (init.get() != State.INITIALIZED) {
      throw new IllegalStateException("Uninitialized");
    }
  }

  ... // Remainder omitted
}


// Serializable subclass of nonserializable stateful class
public class Foo extends AbstractFoo implements Serializable {
  private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
    s.defaultReadObject();

    // Manually deserialize and initialize superclass state
    int x = s.readInt();
    int y = s.readInt();
    initialize(x, y);
  }

  private void writeObject(ObjectOutputStream s) throws IOException {
    s.defaultWriteObject();

    // Manually serialize superclass state
    s.writeInt(getX());
    s.writeInt(getY());
  }

  // Constructor does not use the fancy mechanism
  public Foo(int x, int y) { super(x, y); }

  private static final long serialVersionUID = 1856835860954L;
}


// NOTE Inner classes (Item 22) should not implement Serializable.

// NOTE The default serialized form of an inner class is illdefined.
