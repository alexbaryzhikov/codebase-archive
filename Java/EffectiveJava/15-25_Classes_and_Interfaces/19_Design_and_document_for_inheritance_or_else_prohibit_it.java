
// NOTE The class must document its self-use of overridable methods.

// NOTE A class may have to provide hooks into its internal workings in the form of judiciously
// chosen protected methods.

// NOTE The only way to test a class designed for inheritance is to write subclasses.

// NOTE You must test your class by writing subclasses before you release it.

// NOTE Constructors must not invoke overridable methods.

public class Super {

  // Broken - constructor invokes an overridable method
  public Super() {
    overrideMe();
  }

  public void overrideMe() {
  }
}

// Here’s a subclass that overrides the overrideMe, method which is erroneously invoked by Super’s
// sole constructor.
public final class Sub extends Super {

  private final Date date;  // Blank final, set by constructor

  Sub() {
    date = new Date();
  }

  // Overriding method invoked by superclass constructor
  @Override public void overrideMe() {
    System.out.println(date);
  }

  public static void main(String[] args) {
    Sub sub = new Sub();
    sub.overrideMe();
  }
}

// NOTE Neither clone nor readObject may invoke an overridable method, directly or indirectly.

// NOTE Designing a class for inheritance places substantial limitations on the class.

// NOTE Each time a change is made in such a class, there is a chance that client classes that
// extend the class will break. The best solution to this problem is to prohibit subclassing in
// classes that are not designed and documented to be safely subclassed.
