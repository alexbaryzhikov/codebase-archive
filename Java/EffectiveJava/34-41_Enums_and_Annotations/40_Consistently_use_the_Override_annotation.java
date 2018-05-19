// Can you spot the bug?
public class Bigram {

  private final char first;
  private final char second;

  public Bigram(char first, char second) {
    this.first = first;
    this.second = second;
  }

  public boolean equals(Bigram b) {
    return b.first == first && b.second == second;
  }

  public int hashCode() {
    return 31 * first + second;
  }

  public static void main(String[] args) {
    Set<Bigram> s = new HashSet<>();
    for (int i = 0; i < 10; i++) {
      for (char ch = 'a'; ch <= 'z'; ch++) {
        s.add(new Bigram(ch, ch));
      }
    }
    System.out.println(s.size());
  }
}


@Override public boolean equals(Bigram b) {
  return b.first == first && b.second == second;
}

// Bigram.java:10: method does not override or implement a method from a supertype
//     @Override public boolean equals(Bigram b) {
//     ^


@Override public boolean equals(Object o) {
  if (!(o instanceof Bigram)) {
    return false;
  }
  Bigram b = (Bigram) o;
  return b.first == first && b.second == second;
}


// NOTE Use the Override annotation on every method declaration that you believe to override
// a superclass declaration.
