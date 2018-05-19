public class Stack<E> {
  public Stack();
  public void push(E e);
  public E pop();
  public boolean isEmpty();
}


// pushAll method without wildcard type - deficient!
public void pushAll(Iterable<E> src) {
  for (E e : src) {
    push(e);
  }
}


Stack<Number> numberStack = new Stack<>();
Iterable<Integer> integers = ... ;
numberStack.pushAll(integers);

// StackTest.java:7: pushAll(Iterable<Number>) in Stack<Number> cannot be applied to (Iterable<Integer>)
//     numberStack.pushAll(integers);
//                ^


// Wildcard type for parameter that serves as an E producer
public void pushAll(Iterable<? extends E> src) {
  for (E e : src) {
    push(e);
  }
}


// popAll method without wildcard type - deficient!
public void popAll(Collection<E> dst) {
  while (!isEmpty()) {
    dst.add(pop());
  }
}


Stack<Number> numberStack = new Stack<>();
Collection<Object> objects = ... ;
numberStack.popAll(objects);


// Wildcard type for parameter that serves as an E consumer
public void popAll(Collection<? super E> dst) {
  while (!isEmpty()) {
    dst.add(pop());
  }
}

// -------------------------------------------------------------------------------------------------

// NOTE For maximum flexibility, use wildcard types on input parameters that represent producers
// or consumers.

// NOTE PECS stands for producer-extends, consumer-super.

static <E> E reduce(List<E> list, Function<E> f, E initVal)

// Wildcard type for parameter that serves as an E producer
static <E> E reduce(List<? extends E> list, Function<E> f, E initVal)


public static <E> Set<E> union(Set<E> s1, Set<E> s2)

public static <E> Set<E> union(Set<? extends E> s1, Set<? extends E> s2)


// NOTE Do not use wildcard types as return types.

// NOTE If the user of a class has to think about wildcard types, there is probably something
// wrong with the class’s API.

Set<Integer> integers = ... ;
Set<Double> doubles = ... ;
Set<Number> numbers = union(integers, doubles);

// Union.java:14: incompatible types
// found: Set<Number & Comparable<? extends Number & Comparable<?>>>
// required: Set<Number>
//     Set<Number> numbers = union(integers, doubles);
//                                ^

Set<Number> numbers = Union.<Number>union(integers, doubles);


public static <T extends Comparable<T>> T max(List<T> list)

public static <T extends Comparable<? super T>> T max(List<? extends T> list)


// NOTE Always use Comparable<? super T> in preference to Comparable<T>.

// NOTE Always use Comparator<? super T> in preference to Comparator<T>.


// Won’t compile - wildcards can require change in method body!
public static <T extends Comparable<? super T>> T max(List<? extends T> list) {
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

// Max.java:7: incompatible types
// found: Iterator<capture#591 of ? extends T>
// required: Iterator<T>
//     Iterator<T> i = list.iterator();
//                                  ^

Iterator<? extends T> i = list.iterator();

// -------------------------------------------------------------------------------------------------

// Two possible declarations for the swap method
public static <E> void swap(List<E> list, int i, int j);
public static void swap(List<?> list, int i, int j);


// NOTE If a type parameter appears only once in a method declaration, replace it with a wildcard.


public static void swap(List<?> list, int i, int j) {
  list.set(i, list.set(j, list.get(i)));
}

// Swap.java:5: set(int,capture#282 of ?) in List<capture#282 of ?> cannot be applied to (int,Object)
//     list.set(i, list.set(j, list.get(i)));
//                     ^

public static void swap(List<?> list, int i, int j) {
  swapHelper(list, i, j);
}

// Private helper method for wildcard capture
private static <E> void swapHelper(List<E> list, int i, int j) {
  list.set(i, list.set(j, list.get(i)));
}
