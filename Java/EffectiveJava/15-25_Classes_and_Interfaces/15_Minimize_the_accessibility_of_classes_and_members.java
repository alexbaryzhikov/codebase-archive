
// NOTE Make each class or member as inaccessible as possible.

// NOTE Instance fields should never be public.

// NOTE Classes with public mutable fields are not thread-safe.

// NOTE It is wrong for a class to have a public static final array field, or an accessor that
// returns such a field

// Potential security hole!
public static final Thing[] VALUES = { ... };

// You can make the public array private and add a public immutable list
private static final Thing[] PRIVATE_VALUES = { ... };
public static final List<Thing> VALUES = Collections.unmodifiableList(Arrays.asList(PRIVATE_VALUES));

// Alternatively, you can make the array private and add a public method that returns a copy of
// a private array
private static final Thing[] PRIVATE_VALUES = { ... };

public static final Thing[] values() {
  return PRIVATE_VALUES.clone();
}
