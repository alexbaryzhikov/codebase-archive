// Emulated extensible enum using an interface
public interface Operation {
  double apply(double x, double y);
}


public enum BasicOperation implements Operation {
  PLUS("+")   { public double apply(double x, double y) { return x + y; } },
  MINUS("-")  { public double apply(double x, double y) { return x - y; } },
  TIMES("*")  { public double apply(double x, double y) { return x * y; } },
  DIVIDE("/") { public double apply(double x, double y) { return x / y; } };

  private final String symbol;

  BasicOperation(String symbol) {
    this.symbol = symbol;
  }

  @Override public String toString() {
    return symbol;
  }
}


// Emulated extension enum
public enum ExtendedOperation implements Operation {
  EXP("^")        { public double apply(double x, double y) { return Math.pow(x, y); } },
  REMAINDER("%")  { public double apply(double x, double y) { return x % y; } };

  private final String symbol;

  ExtendedOperation(String symbol) {
    this.symbol = symbol;
  }

  @Override public String toString() {
    return symbol;
  }
}


// Version 1
public static void main(String[] args) {
  double x = Double.parseDouble(args[0]);
  double y = Double.parseDouble(args[1]);
  test(ExtendedOperation.class, x, y);
}


private static <T extends Enum<T> & Operation> void test(Class<T> opSet, double x, double y) {
  for (Operation op : opSet.getEnumConstants()) {
    System.out.printf("%f %s %f = %f%n", x, op, y, op.apply(x, y));
  }
}


// Version 2
public static void main(String[] args) {
  double x = Double.parseDouble(args[0]);
  double y = Double.parseDouble(args[1]);
  test(Arrays.asList(ExtendedOperation.values()), x, y);
}


private static void test(Collection<? extends Operation> opSet, double x, double y) {
  for (Operation op : opSet) {
    System.out.printf("%f %s %f = %f%n", x, op, y, op.apply(x, y));
  }
}


// Both programs above will produce this output when run with command line arguments 2 and 4:

// 4.000000 ^ 2.000000 = 16.000000
// 4.000000 % 2.000000 = 0.000000


// NOTE While you cannot write an extensible enum type, you can emulate it by writing an interface
// to go with a basic enum type that implements the interface.
