
class StringLengthComparator {
  public int compare(String s1, String s2) {
    return s1.length() - s2.length();
  }
}

// As is typical for concrete strategy classes, the StringLengthComparator class is stateless: it
// has no fields, hence all instances of the class are functionally equivalent. Thus it should be
// a singleton to save on unnecessary object creation costs

class StringLengthComparator {
  public static final StringLengthComparator INSTANCE = new StringLengthComparator();

  private StringLengthComparator() { }

  public int compare(String s1, String s2) {
    return s1.length() - s2.length();
  }
}

// Strategy interface
public interface Comparator<T> {
  public int compare(T t1, T t2);
}

class StringLengthComparator implements Comparator<String> {
  ...  // class body is identical to the one shown above
}

Arrays.sort(stringArray, new Comparator<String>() {
  public int compare(String s1, String s2) {
    return s1.length() - s2.length();
  }
});

// Exporting a concrete strategy
class Host {
  private static class StrLenCmp implements Comparator<String>, Serializable {
    public int compare(String s1, String s2) {
      return s1.length() - s2.length();
    }
  }

  // Returned comparator is serializable
  public static final Comparator<String> STRING_LENGTH_COMPARATOR = new StrLenCmp();

  ...  // Bulk of class omitted
}
