// Uses raw types - unacceptable! (Item 23)
public static Set union(Set s1, Set s2) {
  Set result = new HashSet(s1);
  result.addAll(s2);
  return result;
}


// NOTE The type parameter list, which declares the type parameter, goes between the method’s
// modifiers and its return type.


// Generic method
public static <E> Set<E> union(Set<E> s1, Set<E> s2) {
  Set<E> result = new HashSet<>(s1);
  result.addAll(s2);
  return result;
}


// Simple program to exercise generic method
public static void main(String[] args) {
  Set<String> guys = new HashSet<>(Arrays.asList("Tom", "Dick", "Harry"));
  Set<String> stooges = new HashSet<>(Arrays.asList("Larry", "Moe", "Curly"));
  Set<String> aflCio = union(guys, stooges);
  System.out.println(aflCio);
}

// -------------------------------------------------------------------------------------------------

public interface UnaryFunction<T> {
  T apply(T arg);
}


// Generic singleton factory pattern
private static UnaryFunction<Object> IDENTITY_FUNCTION = new UnaryFunction<>() {
  public Object apply(Object arg) { return arg; }
};


// IDENTITY_FUNCTION is stateless and its type parameter is unbounded so it's safe to share one
// instance across all types.
@SuppressWarnings("unchecked")
public static <T> UnaryFunction<T> identityFunction() {
  return (UnaryFunction<T>) IDENTITY_FUNCTION;
}


// Sample program to exercise generic singleton
public static void main(String[] args) {
  String[] strings = { "jute", "hemp", "nylon" };
  UnaryFunction<String> sameString = identityFunction();
  for (String s : strings) {
    System.out.println(sameString.apply(s));
  }

  Number[] numbers = { 1, 2.0, 3L };
  UnaryFunction<Number> sameNumber = identityFunction();
  for (Number n : numbers) {
    System.out.println(sameNumber.apply(n));
  }
}

// -------------------------------------------------------------------------------------------------

public interface Comparable<T> {
  int compareTo(T o);
}

// Using a recursive type bound to express mutual comparability
public static <T extends Comparable<T>> T max(List<T> list) {...}

// Returns the maximum value in a list - uses recursive type bound
public static <T extends Comparable<T>> T max(List<T> list) {
  Iterator<T> i = list.iterator();
  T result = i.next();
  while (i.hasNext()) {
    T t = i.next();
    if (t.compareTo(result) > 0) {
      result = t;
    }
  }
  return result;
}
