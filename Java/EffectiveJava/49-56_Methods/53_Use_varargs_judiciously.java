// Simple use of varargs
static int sum(int... args) {
  int sum = 0;
  for (int arg : args) {
    sum += arg;
  }
  return sum;
}


// The WRONG way to use varargs to pass one or more arguments!
static int min(int... args) {
  if (args.length == 0) {
    throw new IllegalArgumentException("Too few arguments");
  }
  int min = args[0];
  for (int i = 1; i < args.length; i++) {
    if (args[i] < min) {
      min = args[i];
    }
  }
  return min;
}


// The right way to use varargs to pass one or more arguments
static int min(int firstArg, int... remainingArgs) {
  int min = firstArg;
  for (int arg : remainingArgs) {
    if (arg < min) {
      min = arg;
    }
  }
  return min;
}

// -------------------------------------------------------------------------------------------------

List<String> homophones = Arrays.asList("to", "too", "two");


// Obsolete idiom to print an array!
System.out.println(Arrays.asList(myArray));


public static void main(String[] args) {
  int[] digits = { 3, 1, 4, 1, 5, 9, 2, 6, 5, 4 };
  System.out.println(Arrays.asList(digits));
}

// Va.java:6: asList(Object[]) in Arrays can't be applied to (int[])
//     System.out.println(Arrays.asList(digits));
//                              ^


// The right way to print an array
System.out.println(Arrays.toString(myArray));


public static <T> List<T> gather(T... args) {
  return Arrays.asList(args);
}


// NOTE Don’t retrofit every method that has a final array parameter; use varargs only when a call
// really operates on a variable-length sequence of values.


ReturnType1 suspect1(Object... args) { }
<T> ReturnType2 suspect2(T... args) { }


// For performance
public void foo() { }
public void foo(int a1) { }
public void foo(int a1, int a2) { }
public void foo(int a1, int a2, int a3) { }
public void foo(int a1, int a2, int a3, int... rest) { }
