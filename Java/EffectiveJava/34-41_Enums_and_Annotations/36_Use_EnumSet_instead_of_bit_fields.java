// Bit field enumeration constants - OBSOLETE!
public class Text {
  public static final int STYLE_BOLD = 1 << 0;          // 1
  public static final int STYLE_ITALIC = 1 << 1;        // 2
  public static final int STYLE_UNDERLINE = 1 << 2;     // 4
  public static final int STYLE_STRIKETHROUGH = 1 << 3; // 8

  // Parameter is bitwise OR of zero or more STYLE_ constants
  public void applyStyles(int styles) { ... }
}

text.applyStyles(STYLE_BOLD | STYLE_ITALIC);


// EnumSet - a modern replacement for bit fields
public class Text {
  public enum Style { BOLD, ITALIC, UNDERLINE, STRIKETHROUGH }

  // Any Set could be passed in, but EnumSet is clearly best
  public void applyStyles(Set<Style> styles) { ... }
}

text.applyStyles(EnumSet.of(Style.BOLD, Style.ITALIC));


// NOTE just because an enumerated type will be used in sets, there is no reason to represent it
// with bit fields.
