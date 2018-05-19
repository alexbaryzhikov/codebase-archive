// Prints all large anagram groups in a dictionary iteratively
public class Anagrams {
  public static void main(String[] args) throws IOException {
    File dictionary = new File(args[0]);
    int minGroupSize = Integer.parseInt(args[1]);

    Map<String, Set<String>> groups = new HashMap<>();
    try (Scanner s = new Scanner(dictionary)) {
      while (s.hasNext()) {
        String word = s.next();
        groups.computeIfAbsent(alphabetize(word), unused -> new TreeSet<>()).add(word);
      }
    }

    for (Set<String> group : groups.values()) {
      if (group.size() >= minGroupSize) {
        System.out.println(group.size() + ": " + group);
      }
    }
  }

  private static String alphabetize(String s) {
    char[] a = s.toCharArray();
    Arrays.sort(a);
    return new String(a);
  }
}


// Overuse of streams - don't do this!
public class Anagrams {
  public static void main(String[] args) throws IOException {
    Path dictionary = Paths.get(args[0]);
    int minGroupSize = Integer.parseInt(args[1]);

    try (Stream<String> words = Files.lines(dictionary)) {
      words.collect(groupingBy(word ->
          word.chars().sorted()
              .collect(StringBuilder::new, (sb, c) -> sb.append((char) c), StringBuilder::append)
              .toString()))
        .values().stream()
        .filter(group -> group.size() >= minGroupSize)
        .map(group -> group.size() + ": " + group)
        .forEach(System.out::println);
    }
  }
}


// NOTE Overusing streams makes programs hard to read and maintain.


// Tasteful use of streams enhances clarity and conciseness
public class Anagrams {
  public static void main(String[] args) throws IOException {
    Path dictionary = Paths.get(args[0]);
    int minGroupSize = Integer.parseInt(args[1]);

    try (Stream<String> words = Files.lines(dictionary)) {
      words.collect(groupingBy(word -> alphabetize(word)))
        .values().stream()
        .filter(group -> group.size() >= minGroupSize)
        .forEach(g -> System.out.println(g.size() + ": " + g));
    }
  }

  // alphabetize method is the same as in original version
}


// NOTE In the absence of explicit types, careful naming of lambda parameters is essential to the
// readability of stream pipelines.

// NOTE Using helper methods is even more important for readability in stream pipelines than in
// iterative code.


// To demonstrate the hazards of processing char values with streams, consider the following code:
"Hello world!".chars().forEach(System.out::print);  // prints 721011081081113211911111410810033

"Hello world!".chars().forEach(x -> System.out.print((char) x));


// NOTE Refrain from using streams to process char values.

// NOTE Refactor existing code to use streams and use them in new code only where it makes sense
// to do so.


// -------------------------------------------------------------------------------------------------
// Using Iteration:

// • From a code block, you can read or modify any local variable in scope; from a lambda, you can
//   only read final or effectively final variables [JLS 4.12.4], and you can’t modify any local
//   variables.

// • From a code block, you can return from the enclosing method, break or continue an enclosing
//   loop, or throw any checked exception that this method is declared to throw; from a lambda you
//   can do none of these things.

// Using Streams:

// • Uniformly transform sequences of elements
// • Filter sequences of elements
// • Combine sequences of elements using a single operation (for example to add them, concatenate
//   them, or compute their minimum)
// • Accumulate sequences of elements into a collection, perhaps grouping them by some common
//   attribute
// • Search a sequence of elements for an element satisfying some criterion
// -------------------------------------------------------------------------------------------------


static Stream<BigInteger> primes() {
  return Stream.iterate(TWO, BigInteger::nextProbablePrime);
}

// The program to print the first twenty Mersenne primes
public static void main(String[] args) {
  primes().map(p -> TWO.pow(p.intValueExact()).subtract(ONE))
    .filter(mersenne -> mersenne.isProbablePrime(50))
    .limit(20)
    .forEach(System.out::println);
}

// Suppose that we want to precede each Mersenne prime with its exponent (p)
.forEach(mp -> System.out.println(mp.bitLength() + ": " + mp));

// -------------------------------------------------------------------------------------------------

// Iterative Cartesian product computation
private static List<Card> newDeck() {
  List<Card> result = new ArrayList<>();
  for (Suit suit : Suit.values()) {
    for (Rank rank : Rank.values()) {
      result.add(new Card(suit, rank));
    }
  }
  return result;
}


// Stream-based Cartesian product computation
private static List<Card> newDeck() {
  return Stream.of(Suit.values())
    .flatMap(suit ->
      Stream.of(Rank.values())
        .map(rank -> new Card(suit, rank)))
    .collect(toList());
}


// NOTE If you’re not sure whether a task is better served by streams or iteration, try both and
// see which works better.
