// Fails at runtime!
Object[] objectArray = new Long[1];
objectArray[0] = "I don't fit in"; // Throws ArrayStoreException

// Won't compile!
List<Object> ol = new ArrayList<Long>(); // Incompatible types
ol.add("I don't fit in");

// -------------------------------------------------------------------------------------------------

// Why generic array creation is illegal - won't compile!
List<String>[] stringLists = new List<String>[1]; // (1)
List<Integer> intList = Arrays.asList(42);        // (2)
Object[] objects = stringLists;                   // (3)
objects[0] = intList;                             // (4)
String s = stringLists[0].get(0);                 // (5)

// -------------------------------------------------------------------------------------------------

// Reduction without generics, and with concurrency flaw!
static Object reduce(List list, Function f, Object initVal) {
  synchronized(list) {
    Object result = initVal;
    for (Object o : list) {
      result = f.apply(result, o);
    }
    return result;
  }
}

interface Function {
  Object apply(Object arg1, Object arg2);
}

// Reduction without generics or concurrency flaw
static Object reduce(List list, Function f, Object initVal) {
  Object[] snapshot = list.toArray(); // Locks list internally
  Object result = initVal;
  for (Object o : list) {
    result = f.apply(result, o);
  }
  return result;
}

// -------------------------------------------------------------------------------------------------

interface Function<T> {
  T apply(T arg1, T arg2);
}

// Naive generic version of reduction - won't compile!
static <E> E reduce(List<E> list, Function<E> f, E initVal) {
  E[] snapshot = list.toArray(); // Locks list
  E result = initVal;
  for (E e : snapshot) {
    result = f.apply(result, e);
  }
  return result;
}

// If you try to compile this method, you’ll get the following error:

// Reduce.java:12: incompatible types found: Object[], required: E[]
//     E[] snapshot = list.toArray(); // Locks list
//                                ^

// No big deal, you say, I’ll cast the Object array to an E array:

E[] snapshot = (E[]) list.toArray();

// That gets rid of the error, but now you get a warning:

// Reduce.java:12: warning: [unchecked] unchecked cast found: Object[], required: E[]
//     E[] snapshot = (E[]) list.toArray(); // Locks list
//                                      ^

// List-based generic reduction
static <E> E reduce(List<E> list, Function<E> f, E initVal) {
  List<E> snapshot;
  synchronized(list) {
    snapshot = new ArrayList<E>(list);
  }
  E result = initVal;
  for (E e : snapshot) {
    result = f.apply(result, e);
  }
  return result;
}
