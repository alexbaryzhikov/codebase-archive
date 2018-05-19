// warning: [unchecked] Possible heap pollution from parameterized vararg type List<String>


// Mixing generics and varargs can violate type safety!
static void dangerous(List<String>... stringLists) {
  List<Integer> intList = List.of(42);
  Object[] objects = stringLists;
  objects[0] = intList; // Heap pollution
  String s = stringLists[0].get(0); // ClassCastException
}


// NOTE It is unsafe to store a value in a generic varargs array parameter.

// NOTE The SafeVarargs annotation constitutes a promise by the author of a method that it is
// typesafe.


// UNSAFE - Exposes a reference to its generic parameter array!
static <T> T[] toArray(T... args) {
  return args;
}


static <T> T[] pickTwo(T a, T b, T c) {
  switch(ThreadLocalRandom.current().nextInt(3)) {
    case 0: return toArray(a, b);
    case 1: return toArray(a, c);
    case 2: return toArray(b, c);
  }
  throw new AssertionError(); // Can't get here
}

public static void main(String[] args) {
  String[] attributes = pickTwo("Good", "Fast", "Cheap");
}

// NOTE It is unsafe to give another method access to a generic varargs parameter array.


// Safe method with a generic varargs parameter
@SafeVarargs
static <T> List<T> flatten(List<? extends T>... lists) {
  List<T> result = new ArrayList<>();
  for (List<? extends T> list : lists) {
    result.addAll(list);
  }
  return result;
}


// NOTE Use @SafeVarargs on every method with a varargs parameter of a generic or parameterized
// type.


// List as a typesafe alternative to a generic varargs parameter
static <T> List<T> flatten(List<List<? extends T>> lists) {
  List<T> result = new ArrayList<>();
  for (List<? extends T> list : lists) {
    result.addAll(list);
  }
  return result;
}

audience = flatten(List.of(friends, romans, countrymen));


static <T> List<T> pickTwo(T a, T b, T c) {
  switch(rnd.nextInt(3)) {
    case 0: return List.of(a, b);
    case 1: return List.of(a, c);
    case 2: return List.of(b, c);
  }
  throw new AssertionError();
}

public static void main(String[] args) {
  List<String> attributes = pickTwo("Good", "Fast", "Cheap");
}
