
// NOTE To make a class immutable, follow these five rules:
// 1. Don’t provide any methods that modify the object’s state.
// 2. Ensure that the class can’t be extended.
// 3. Make all fields final.
// 4. Make all fields private.
// 5. Ensure exclusive access to any mutable components.

public final class Complex {

  private final double re;
  private final double im;

  public Complex(double re, double im) {
    this.re = re;
    this.im = im;
  }

  // Accessors with no corresponding mutators
  public double realPart() { return re; }
  public double imaginaryPart() { return im; }

  public Complex add(Complex c) {
    return new Complex(re + c.re, im + c.im);
  }

  public Complex subtract(Complex c) {
    return new Complex(re - c.re, im - c.im);
  }

  public Complex multiply(Complex c) {
    return new Complex(re * c.re - im * c.im,
    re * c.im + im * c.re);
  }

  public Complex divide(Complex c) {
    double tmp = c.re * c.re + c.im * c.im;
    return new Complex((re * c.re + im * c.im) / tmp, (im * c.re - re * c.im) / tmp);
  }

  @Override public boolean equals(Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof Complex)) {
      return false;
    }
    Complex c = (Complex) o;

    // See page 43 to find out why we use compare instead of ==
    return Double.compare(re, c.re) == 0 && Double.compare(im, c.im) == 0;
  }

  @Override public int hashCode() {
    int result = 17 + hashDouble(re);
    result = 31 * result + hashDouble(im);
    return result;
  }

  private int hashDouble(double val) {
    long longBits = Double.doubleToLongBits(re);
    return (int) (longBits ^ (longBits >>> 32));
  }

  @Override public String toString() {
    return "(" + re + " + " + im + "i)";
  }
}

// NOTE Immutable objects are simple.

// NOTE Immutable objects are inherently thread-safe; they require no synchronization.

// NOTE Immutable objects can be shared freely.

// The Complex class might provide these constants
public static final Complex ZERO = new Complex(0, 0);
public static final Complex ONE = new Complex(1, 0);
public static final Complex I = new Complex(0, 1);

// NOTE Not only can you share immutable objects, but you can share their internals.

// NOTE Immutable objects make great building blocks for other objects.

// NOTE The only real disadvantage of immutable classes is that they require a separate object
// for each distinct value.

BigInteger moby = ...;
moby = moby.flipBit(0);

// -------------------------------------------------------------------------------------------------

// Immutable class with static factories instead of constructors
public class Complex {

  private final double re;
  private final double im;

  private Complex(double re, double im) {
    this.re = re;
    this.im = im;
  }

  public static Complex valueOf(double re, double im) {
    return new Complex(re, im);
  }

  ...  // Remainder unchanged
}

// Provides a means of creating a complex number based on its polar coordinates
public static Complex valueOfPolar(double r, double theta) {
  return new Complex(r * Math.cos(theta), r * Math.sin(theta));
}

// If you write a class whose security depends on the immutability of a BigInteger or BigDecimal
// argument from an untrusted client, you must check to see that the argument is a “real”
// BigInteger or BigDecimal, rather than an instance of an untrusted subclass. If it is the latter,
// you must defensively copy it under the assumption that it might be mutable
public static BigInteger safeInstance(BigInteger val) {
  if (val.getClass() != BigInteger.class) {
    return new BigInteger(val.toByteArray());
  }
  return val;
}

// -------------------------------------------------------------------------------------------------

// NOTE Classes should be immutable unless there’s a very good reason to make them mutable.

// NOTE If a class cannot be made immutable, limit its mutability as much as possible.

// NOTE Make every field final unless there is a compelling reason to make it nonfinal.
