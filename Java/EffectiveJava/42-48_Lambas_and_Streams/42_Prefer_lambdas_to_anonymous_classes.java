// Anonymous class instance as a function object - obsolete!
Collections.sort(words, new Comparator<String>() {
  public int compare(String s1, String s2) {
    return Integer.compare(s1.length(), s2.length());
  }
});


// Lambda expression as function object (replaces anonymous class)
Collections.sort(words, (s1, s2) -> Integer.compare(s1.length(), s2.length()));


// NOTE Omit the types of all lambda parameters unless their presence makes your program clearer.

// Comparator construction method
Collections.sort(words, comparingInt(String::length));

words.sort(comparingInt(String::length));

// -------------------------------------------------------------------------------------------------

// Enum type with constant-specific class bodies & data (Item 34)
public enum Operation {
  PLUS("+") {
    public double apply(double x, double y) { return x + y; }
  },
  MINUS("-") {
    public double apply(double x, double y) { return x - y; }
  },
  TIMES("*") {
    public double apply(double x, double y) { return x * y; }
  },
  DIVIDE("/") {
    public double apply(double x, double y) { return x / y; }
  };

  private final String symbol;

  Operation(String symbol) { this.symbol = symbol; }

  @Override public String toString() { return symbol; }

  public abstract double apply(double x, double y);
}


// Enum with function object fields & constant-specific behavior
public enum Operation {
  PLUS ("+", (x, y) -> x + y),
  MINUS ("-", (x, y) -> x - y),
  TIMES ("*", (x, y) -> x * y),
  DIVIDE("/", (x, y) -> x / y);

  private final String symbol;
  private final DoubleBinaryOperator op;

  Operation(String symbol, DoubleBinaryOperator op) {
    this.symbol = symbol;
    this.op = op;
  }

  @Override public String toString() { return symbol; }

  public double apply(double x, double y) {
    return op.applyAsDouble(x, y);
  }
}


// NOTE Lambdas lack names and documentation; if a computation isn’t self-explanatory, or exceeds
// a few lines, don’t put it in a lambda.

// NOTE You should rarely, if ever, serialize a lambda.

// NOTE Don’t use anonymous classes for function objects unless you have to create instances of
// types that aren’t functional interfaces.
