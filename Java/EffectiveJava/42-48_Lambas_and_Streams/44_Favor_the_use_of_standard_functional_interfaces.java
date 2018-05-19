
protected boolean removeEldestEntry(Map.Entry<K,V> eldest) {
  return size() > 100;
}


// Unnecessary functional interface; use a standard one instead.
@FunctionalInterface interface EldestEntryRemovalFunction<K,V>{
  boolean remove(Map<K,V> map, Map.Entry<K,V> eldest);
}


// NOTE If one of the standard functional interfaces does the job, you should generally use it in
// preference to a purpose-built functional interface.


// -------------------------------------------------------------------------------------------------
// Interface            Function Signature      Example
//
// UnaryOperator<T>     T apply(T t)            String::toLowerCase
// BinaryOperator<T>    T apply(T t1, T t2)     BigInteger::add
// Predicate<T>         boolean test(T t)       Collection::isEmpty
// Function<T,R>        R apply(T t)            Arrays::asList
// Supplier<T>          T get()                 Instant::now
// Consumer<T>          void accept(T t)        System.out::println
// -------------------------------------------------------------------------------------------------


// NOTE Don’t be tempted to use basic functional interfaces with boxed primitives instead of
// primitive functional interfaces.


// NOTE Always annotate your functional interfaces with the @FunctionalInterface annotation.
