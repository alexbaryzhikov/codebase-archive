
// NOTE When to NOT override `equals`:

// - Each instance of the class is inherently unique.

// - You don’t care whether the class provides a “logical equality” test.

// - A superclass has already overridden `equals`, and the superclass behavior is appropriate for
//   this class.

// - The class is private or package-private, and you are certain that its `equals` method will
//   never be invoked.

@Override public boolean equals(Object o) {
  throw new AssertionError();  // Method is never called
}

// -------------------------------------------------------------------------------------------------

// NOTE The equals method implements an equivalence relation. It is:

// - Reflexive: For any non-null reference value x, x.equals(x) must return `true`.

// - Symmetric: For any non-null reference values x and y, x.equals(y) must return `true` if and
//   only if y.equals(x) returns `true`.

// - Transitive: For any non-null reference values x, y, z, if x.equals(y) returns `true` and
//   y.equals(z) returns `true`, then x.equals(z) must return `true`.

// - Consistent: For any non-null reference values x and y, multiple invocations of x.equals(y)
//   consistently return `true` or consistently return `false`, provided no information used in
//   `equals` comparisons on the objects is modified.

// - For any non-null reference value x, x.equals(null) must return `false`.

// -------------------------------------------------------------------------------------------------

// Symmetry

// Broken - violates symmetry!
public final class CaseInsensitiveString {

  private final String s;

  public CaseInsensitiveString(String s) {
    if (s == null) {
      throw new NullPointerException();
    }
    this.s = s;
  }

  // Broken - violates symmetry!
  @Override public boolean equals(Object o) {
    if (o instanceof CaseInsensitiveString) {
      return s.equalsIgnoreCase(((CaseInsensitiveString) o).s);
    }
    if (o instanceof String) {  // One-way interoperability!
      return s.equalsIgnoreCase((String) o);
    }
    return false;
  }

  ...  // Remainder omitted
}

CaseInsensitiveString cis = new CaseInsensitiveString("Polish");
String s = "polish";

List<CaseInsensitiveString> list = new ArrayList<>();
list.add(cis);

// NOTE Once you’ve violated the equals contract, you simply don’t know how other objects will
// behave when confronted with your object.

// Refactored method
@Override public boolean equals(Object o) {
  return o instanceof CaseInsensitiveString && ((CaseInsensitiveString) o).s.equalsIgnoreCase(s);
}

// -------------------------------------------------------------------------------------------------

// Transitivity

public class Point {

  private final int x;
  private final int y;

  public Point(int x, int y) {
    this.x = x;
    this.y = y;
  }

  @Override public boolean equals(Object o) {
    if (!(o instanceof Point)) {
      return false;
    }
    Point p = (Point) o;
    return p.x == x && p.y == y;
  }

  ...  // Remainder omitted
}

public class ColorPoint extends Point {

  private final Color color;

  public ColorPoint(int x, int y, Color color) {
    super(x, y);
    this.color = color;
  }

// Broken - violates symmetry!
@Override public boolean equals(Object o) {
  if (!(o instanceof ColorPoint)) {
    return false;
  }
  return super.equals(o) && ((ColorPoint) o).color == color;
}

Point p = new Point(1, 2);
ColorPoint cp = new ColorPoint(1, 2, Color.RED);

// Broken - violates transitivity!
@Override public boolean equals(Object o) {
  if (!(o instanceof Point)) {
    return false;
  }
  // If o is a normal Point, do a color-blind comparison
  if (!(o instanceof ColorPoint)) {
    return o.equals(this);
  }
  // o is a ColorPoint; do a full comparison
  return super.equals(o) && ((ColorPoint) o).color == color;
}

ColorPoint p1 = new ColorPoint(1, 2, Color.RED);
Point p2 = new Point(1, 2);
ColorPoint p3 = new ColorPoint(1, 2, Color.BLUE);

// NOTE There is no way to extend an instantiable class and add a value component while preserving
// the equals contract

// Broken - violates Liskov substitution principle (page 40)
@Override public boolean equals(Object o) {
  if (o == null || o.getClass() != getClass()) {
    return false;
  }
  Point p = (Point) o;
  return p.x == x && p.y == y;
}

// Initialize UnitCircle to contain all Points on the unit circle
private static final Set<Point> unitCircle;
static {
  unitCircle = new HashSet<Point>();
  unitCircle.add(new Point( 1, 0));
  unitCircle.add(new Point( 0, 1));
  unitCircle.add(new Point(-1, 0));
  unitCircle.add(new Point( 0, -1));
}

public static boolean onUnitCircle(Point p) {
  return unitCircle.contains(p);
}

// Keep track of how many instances have been created
// onUnitCircle method will return false regardless of the CounterPoint instance’s x and y values
public class CounterPoint extends Point {

  private static final AtomicInteger counter = new AtomicInteger();

  public CounterPoint(int x, int y) {
    super(x, y);
    counter.incrementAndGet();
  }

  public int numberCreated() { return counter.get(); }
}

// Adds a value component without violating the equals contract
public class ColorPoint {

  private final Point point;
  private final Color color;

  public ColorPoint(int x, int y, Color color) {
    if (color == null) {
      throw new NullPointerException();
    }
    point = new Point(x, y);
    this.color = color;
  }

  /**
   * Returns the point-view of this color point.
   */
  public Point asPoint() {
    return point;
  }

  @Override public boolean equals(Object o) {
    if (!(o instanceof ColorPoint)) {
      return false;
    }
    ColorPoint cp = (ColorPoint) o;
    return cp.point.equals(point) && cp.color.equals(color);
  }

  ...  // Remainder omitted
}

// -------------------------------------------------------------------------------------------------

// Consistency

// NOTE Do not write an equals method that depends on unreliable resources.

// -------------------------------------------------------------------------------------------------

// "Non-nullity"

@Override public boolean equals(Object o) {
  if (o == null) {  // This test is unnecessary
    return false;
  }
  ...
}

@Override public boolean equals(Object o) {
  if (!(o instanceof MyType)) {
    return false;
  }
  MyType mt = (MyType) o;
  ...
}

// -------------------------------------------------------------------------------------------------

// NOTE A recipe for a high-quality equals method:

// 1. Use the `==` operator to check if the argument is a reference to this object.

// 2. Use the `instanceof` operator to check if the argument has the correct type.

// 3. Cast the argument to the correct type.

// 4. For each "significant" field in the class, check if that field of the argument matches the
//    corresponding field of this object.

  (field == o.field || (field != null && field.equals(o.field)))

// 5. When you are finished writing your equals method, ask yourself three questions:
//    Is it symmetric? Is it transitive? Is it consistent?


// NOTE Here are a few final caveats:

// - Always override `hashCode` when you override `equals`.

// - Don’t try to be too clever.

// - Don’t substitute another type for Object in the equals declaration.

  public boolean equals(MyClass o) {  // Overloads rather than overrides `equals`
    ...
  }

  @Override public boolean equals(MyClass o) {  // Compile-time error
    ...
  }
