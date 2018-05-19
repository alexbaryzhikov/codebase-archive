// Broken comparator - can you spot the flaw?
Comparator<Integer> naturalOrder = new Comparator<Integer>() {
  public int compare(Integer first, Integer second) {
    return first < second ? -1 : (first == second ? 0 : 1);
  }
};


// NOTE Applying the == operator to boxed primitives is almost always wrong.


Comparator<Integer> naturalOrder = new Comparator<Integer>() {
  public int compare(Integer first, Integer second) {
    int f = first;  // Auto-unboxing
    int s = second; // Auto-unboxing
    return f < s ? -1 : (f == s ? 0 : 1); // No unboxing
  }
};

// -------------------------------------------------------------------------------------------------

// NOTE When your program does unboxing, it can throw a NullPointerException.

public class Unbelievable {
  static Integer i;

  public static void main(String[] args) {
    if (i == 42) {  // NullPointerException
      System.out.println("Unbelievable");
    }
  }
}


// NOTE When you mix primitives and boxed primitives in a single operation, the boxed primitive is
// autounboxed


// Hideously slow program! Can you spot the object creation?
public static void main(String[] args) {
  Long sum = 0L;
  for (long i = 0; i < Integer.MAX_VALUE; i++) {
    sum += i;
  }
  System.out.println(sum);
}


// NOTE Autoboxing reduces the verbosity, but not the danger, of using boxed primitives.
