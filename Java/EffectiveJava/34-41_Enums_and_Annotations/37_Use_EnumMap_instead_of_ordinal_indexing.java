public class Herb {
  public enum Type { ANNUAL, PERENNIAL, BIENNIAL }

  private final String name;
  private final Type type;

  Herb(String name, Type type) {
    this.name = name;
    this.type = type;
  }

  @Override public String toString() {
    return name;
  }
}


// Using ordinal() to index an array - DON'T DO THIS!
Herb[] garden = ... ;

// Indexed by Herb.Type.ordinal()
Set<Herb>[] herbsByType = (Set<Herb>[]) new Set[Herb.Type.values().length];
for (int i = 0; i < herbsByType.length; i++) {
  herbsByType[i] = new HashSet<Herb>();
}

for (Herb h : garden) {
  herbsByType[h.type.ordinal()].add(h);
}

// Print the results
for (int i = 0; i < herbsByType.length; i++) {
  System.out.printf("%s: %s%n", Herb.Type.values()[i], herbsByType[i]);
}


// Using an EnumMap to associate data with an enum
Map<Herb.Type, Set<Herb>> herbsByType = new EnumMap<>(Herb.Type.class);
for (Herb.Type t : Herb.Type.values()) {
  herbsByType.put(t, new HashSet<Herb>());
}
for (Herb h : garden) {
  herbsByType.get(h.type).add(h);
}
System.out.println(herbsByType);

// -------------------------------------------------------------------------------------------------

// Using ordinal() to index array of arrays - DON'T DO THIS!
public enum Phase {
  SOLID, LIQUID, GAS;

  public enum Transition {
    MELT, FREEZE, BOIL, CONDENSE, SUBLIME, DEPOSIT;

    // Rows indexed by src-ordinal, cols by dst-ordinal
    private static final Transition[][] TRANSITIONS = {
      { null, MELT, SUBLIME },
      { FREEZE, null, BOIL },
      { DEPOSIT, CONDENSE, null }
    };

    // Returns the phase transition from one phase to another
    public static Transition from(Phase src, Phase dst) {
      return TRANSITIONS[src.ordinal()][dst.ordinal()];
    }
  }
}


// Using a nested EnumMap to associate data with enum pairs
public enum Phase {
  SOLID, LIQUID, GAS;

  public enum Transition {
    MELT(SOLID, LIQUID),
    FREEZE(LIQUID, SOLID),
    BOIL(LIQUID, GAS),
    CONDENSE(GAS, LIQUID),
    SUBLIME(SOLID, GAS),
    DEPOSIT(GAS, SOLID);

    final Phase src;
    final Phase dst;

    Transition(Phase src, Phase dst) {
      this.src = src;
      this.dst = dst;
    }

    // Initialize the phase transition map
    private static final Map<Phase, Map<Phase,Transition>> m = new EnumMap<>(Phase.class);

    static {
      for (Phase p : Phase.values()) {
        m.put(p,new EnumMap<Phase,Transition>(Phase.class));
      }
      for (Transition trans : Transition.values()) {
        m.get(trans.src).put(trans.dst, trans);
      }
    }

    public static Transition from(Phase src, Phase dst) {
      return m.get(src).get(dst);
    }
  }
}


// NOTE It is rarely appropriate to use ordinals to index arrays: use EnumMap instead.
