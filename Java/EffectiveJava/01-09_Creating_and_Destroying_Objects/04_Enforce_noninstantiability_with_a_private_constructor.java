
// NOTE Attempting to enforce noninstantiability by making a class abstract does not work.

// Noninstantiable utility class

public class UtilityClass {
  // Suppress default constructor for noninstantiability
  private UtilityClass() {
    throw new AssertionError();
  }
  ... // Remainder omitted
}
