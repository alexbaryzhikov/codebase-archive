// Broken! - What does this program print?
public class CollectionClassifier {

  public static String classify(Set<?> s) {
    return "Set";
  }

  public static String classify(List<?> lst) {
    return "List";
  }

  public static String classify(Collection<?> c) {
    return "Unknown Collection";
  }

  public static void main(String[] args) {
    Collection<?>[] collections = {
      new HashSet<String>(),
      new ArrayList<BigInteger>(),
      new HashMap<String, String>().values()
    };
    for (Collection<?> c : collections)
    System.out.println(classify(c));
  }
}


// NOTE The choice of which overloading to invoke is made at compile time.

// NOTE Selection among overloaded methods is static, while selection among overridden methods is
// dynamic


class Wine {
  String name() { return "wine"; }
}

class SparklingWine extends Wine {
  @Override String name() { return "sparkling wine"; }
}

class Champagne extends SparklingWine {
  @Override String name() { return "champagne"; }
}

public class Overriding {
  public static void main(String[] args) {
    Wine[] wines = {
      new Wine(), new SparklingWine(), new Champagne()
    };
    for (Wine wine : wines) {
      System.out.println(wine.name());
    }
  }
}


public static String classify(Collection<?> c) {
  return c instanceof Set ? "Set" : c instanceof List ? "List" : "Unknown Collection";
}


// NOTE Avoid confusing uses of overloading.

// NOTE A safe, conservative policy is never to export two overloadings with the same number of
// parameters.


public class SetList {
  public static void main(String[] args) {
    Set<Integer> set = new TreeSet<Integer>();
    List<Integer> list = new ArrayList<Integer>();

    for (int i = -3; i < 3; i++) {
      set.add(i);
      list.add(i);
    }

    for (int i = 0; i < 3; i++) {
      set.remove(i);
      list.remove(i);
    }

    System.out.println(set + " " + list);
  }
}

// Prints [-3, -2, -1] [-2, 0, 2]

for (int i = 0; i < 3; i++) {
  set.remove(i);
  list.remove((Integer) i);  // or remove(Integer.valueOf(i))
}
