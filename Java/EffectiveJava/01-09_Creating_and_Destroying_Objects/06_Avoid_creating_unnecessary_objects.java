
String s = new String("stringette");  // DON'T DO THIS!

String s = "stringette";

// -------------------------------------------------------------------------------------------------

public class Person {

  private final Date birthDate;
  // Other fields, methods, and constructor omitted

  // DON'T DO THIS!
  public boolean isBabyBoomer() {
    // Unnecessary allocation of expensive object
    Calendar gmtCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
    gmtCal.set(1946, Calendar.JANUARY, 1, 0, 0, 0);
    Date boomStart = gmtCal.getTime();
    gmtCal.set(1965, Calendar.JANUARY, 1, 0, 0, 0);
    Date boomEnd = gmtCal.getTime();
    return birthDate.compareTo(boomStart) >= 0 && birthDate.compareTo(boomEnd) < 0;
  }
}


// Avoid this inefficiency with a static initializer

class Person {

  private final Date birthDate;
  // Other fields, methods, and constructor omitted

  /**
   * The starting and ending dates of the baby boom.
   */
  private static final Date BOOM_START;
  private static final Date BOOM_END;

  static {
    Calendar gmtCal = Calendar.getInstance(TimeZone.getTimeZone("GMT"));
    gmtCal.set(1946, Calendar.JANUARY, 1, 0, 0, 0);
    BOOM_START = gmtCal.getTime();
    gmtCal.set(1965, Calendar.JANUARY, 1, 0, 0, 0);
    BOOM_END = gmtCal.getTime();
  }

  public boolean isBabyBoomer() {
    return birthDate.compareTo(BOOM_START) >= 0 && birthDate.compareTo(BOOM_END) < 0;
  }
}

// -------------------------------------------------------------------------------------------------

// Hideously slow program! Can you spot the object creation?

public static void main(String[] args) {
  Long sum = 0L;
  for (long i = 0; i < Integer.MAX_VALUE; i++) {
    sum += i;
  }
  System.out.println(sum);
}

// NOTE Prefer primitives to boxed primitives, and watch out for unintentional autoboxing.
