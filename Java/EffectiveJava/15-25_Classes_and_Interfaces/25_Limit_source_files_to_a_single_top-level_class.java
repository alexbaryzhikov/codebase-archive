public class Main {
  public static void main(String[] args) {
    System.out.println(Utensil.NAME + Dessert.NAME);
  }
}

// Utensil.java
// Two classes defined in one file. Don't ever do this!
class Utensil {
  static final String NAME = "pan";
}

class Dessert {
  static final String NAME = "cake";
}


// Dessert.java
// Two classes defined in one file. Don't ever do this!
class Utensil {
  static final String NAME = "pot";
}

class Dessert {
  static final String NAME = "pie";
}


// Static member classes instead of multiple top-level classes
public class Test {
  public static void main(String[] args) {
    System.out.println(Utensil.NAME + Dessert.NAME);
  }

  private static class Utensil {
    static final String NAME = "pan";
  }

  private static class Dessert {
    static final String NAME = "cake";
  }
}


// NOTE Never put multiple top-level classes or interfaces in a single source file.
