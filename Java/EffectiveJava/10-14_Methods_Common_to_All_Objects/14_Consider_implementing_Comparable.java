
// Sorting an array of objects that implement Comparable
Arrays.sort(a);

// Prints an alphabetized list of its command-line arguments with duplicates eliminated
public class WordList {
  public static void main(String[] args) {
    Set<String> s = new TreeSet<String>();
    Collections.addAll(s, args);
    System.out.println(s);
  }
}

// -------------------------------------------------------------------------------------------------

public interface Comparable<T> {
  int compareTo(T t);
}

// sgn(expression) designates the mathematical signum function, which is defined to return
// -1, 0, or 1, according to whether the value of expression is negative, zero, or positive
sgn(x.compareTo(y)) == -sgn(y.compareTo(x))  // required
(x.compareTo(y) > 0 && y.compareTo(z) > 0) implies x.compareTo(z) > 0  // required
x.compareTo(y) == 0 implies that sgn(x.compareTo(z)) == sgn(y.compareTo(z))  // required
(x.compareTo(y) == 0) == (x.equals(y))  // optional, strongly recommended

// -------------------------------------------------------------------------------------------------

public final class CaseInsensitiveString implements Comparable<CaseInsensitiveString> {

  public int compareTo(CaseInsensitiveString cis) {
    return String.CASE_INSENSITIVE_ORDER.compare(s, cis.s);
  }

  ...  // Remainder omitted
}

public int compareTo(PhoneNumber pn) {
  // Compare area codes
  if (areaCode < pn.areaCode) {
    return -1;
  }
  if (areaCode > pn.areaCode) {
    return 1;
  }

  // Area codes are equal, compare prefixes
  if (prefix < pn.prefix) {
    return -1;
  }
  if (prefix > pn.prefix) {
    return 1;
  }

  // Area codes and prefixes are equal, compare line numbers
  if (lineNumber < pn.lineNumber) {
    return -1;
  }
  if (lineNumber > pn.lineNumber) {
    return 1;
  }

  return 0;  // All fields are equal
}

// Improved version
public int compareTo(PhoneNumber pn) {
  // Compare area codes
  int areaCodeDiff = areaCode - pn.areaCode;
  if (areaCodeDiff != 0) {
    return areaCodeDiff;
  }

  // Area codes are equal, compare prefixes
  int prefixDiff = prefix - pn.prefix;
  if (prefixDiff != 0) {
    return prefixDiff;
  }

  // Area codes and prefixes are equal, compare line numbers
  return lineNumber - pn.lineNumber;
}
