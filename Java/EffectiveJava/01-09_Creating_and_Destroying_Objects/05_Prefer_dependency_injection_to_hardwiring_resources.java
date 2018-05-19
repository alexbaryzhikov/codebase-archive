// Inappropriate use of static utility - inflexible & untestable!
public class SpellChecker {
  private static final Lexicon dictionary = ...;

  private SpellChecker() {} // Noninstantiable

  public static boolean isValid(String word) { ... }
  public static List<String> suggestions(String typo) { ... }
}


// Inappropriate use of singleton - inflexible & untestable!
public class SpellChecker {
  private final Lexicon dictionary = ...;

  private SpellChecker(...) {}
  public static INSTANCE = new SpellChecker(...);

  public boolean isValid(String word) { ... }
  public List<String> suggestions(String typo) { ... }
}


// NOTE Static utility classes and singletons are inappropriate for classes whose behavior is
// parameterized by an underlying resource.

// NOTE Pass the resource into the constructor when creating a new instance.


// Dependency injection provides flexibility and testability
public class SpellChecker {
  private final Lexicon dictionary;

  public SpellChecker(Lexicon dictionary) {
    this.dictionary = Objects.requireNonNull(dictionary);
  }

  public boolean isValid(String word) { ... }
  public List<String> suggestions(String typo) { ... }
}


// Pass a resource factory to the constructor
Mosaic create(Supplier<? extends Tile> tileFactory) { ... }
