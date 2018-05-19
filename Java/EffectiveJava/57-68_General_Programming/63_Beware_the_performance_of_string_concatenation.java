// NOTE Using the string concatenation operator repeatedly to concatenate n strings requires time
// quadratic in n.

// Inappropriate use of string concatenation - Performs horribly!
public String statement() {
  String result = "";
  for (int i = 0; i < numItems(); i++) {
    result += lineForItem(i); // String concatenation
  }
  return result;
}


// NOTE To achieve acceptable performance, use a StringBuilder in place of a String.


public String statement() {
  StringBuilder b = new StringBuilder(numItems() * LINE_WIDTH);
  for (int i = 0; i < numItems(); i++) {
    b.append(lineForItem(i));
  }
  return b.toString();
}
