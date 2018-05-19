// NOTE Under most circumstances, normal initialization is preferable to lazy initialization.


// Normal initialization of an instance field
private final FieldType field = computeFieldValue();


// NOTE If you use lazy initialization to break an initialization circularity, use a synchronized
// accessor


// Lazy initialization of instance field - synchronized accessor
private FieldType field;

synchronized FieldType getField() {
  if (field == null) {
    field = computeFieldValue();
  }
  return field;
}


// NOTE If you need to use lazy initialization for performance on a static field, use the lazy
// initialization holder class idiom.


// Lazy initialization holder class idiom for static fields
private static class FieldHolder {
  static final FieldType field = computeFieldValue();
}

static FieldType getField() { return FieldHolder.field; }


// NOTE If you need to use lazy initialization for performance on an instance field, use the
// double-check idiom.


// Double-check idiom for lazy initialization of instance fields
private volatile FieldType field;

FieldType getField() {
  FieldType result = field;
  if (result == null) { // First check (no locking)
    synchronized(this) {
      result = field;
      if (result == null) { // Second check (with locking)
        field = result = computeFieldValue();
      }
    }
  }
  return result;
}


// Single-check idiom - can cause repeated initialization!
private volatile FieldType field;

private FieldType getField() {
  FieldType result = field;
  if (result == null) {
    field = result = computeFieldValue();
  }
  return result;
}
