private static final Random rnd = new Random();

// Common but deeply flawed!
static int random(int n) {
  return Math.abs(rnd.nextInt()) % n;
}


// Prints a number close to 666,666
public static void main(String[] args) {
  int n = 2 * (Integer.MAX_VALUE / 3);
  int low = 0;
  for (int i = 0; i < 1000000; i++) {
    if (random(n) < n/2) {
      low++;
    }
  }
  System.out.println(low);
}


// NOTE By using a standard library, you take advantage of the knowledge of the experts who wrote
// it and the experience of those who used it before you.

// NOTE Numerous features are added to the libraries in every major release, and it pays to keep
// abreast of these additions.

// NOTE Every programmer should be familiar with the contents of java.lang, java.util, and, to
// a lesser extent, java.io.

// NOTE java.util.concurrent
