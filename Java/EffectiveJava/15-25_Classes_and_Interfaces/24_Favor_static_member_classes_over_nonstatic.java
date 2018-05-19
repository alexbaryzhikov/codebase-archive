
// Typical use of a nonstatic member class
public class MySet<E> extends AbstractSet<E> {

  ...  // Bulk of the class omitted

  public Iterator<E> iterator() {
    return new MyIterator();
  }

  private class MyIterator implements Iterator<E> {
    ...
  }
}

// NOTE If you declare a member class that does not require access to an enclosing instance, always
// put the static modifier in its declaration
