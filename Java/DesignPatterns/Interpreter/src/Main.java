/* --------------------------------------------------------
 * Interpreter
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Given a language, define a representation for its grammar along with an interpreter that uses
 *   the representation to interpret sentences in the language
 * - Map a domain to a language, the language to a grammar, and the grammar to a hierarchical
 *   object-oriented design
 *
 * Applicability
 *
 * - The Interpreter pattern is used in defining grammar, tokenizing input and storing it
 * - A specific area where Interpreter can be used are the rules engines
 * - The Interpreter pattern can be used to add functionality to the composite pattern
 */

import java.util.ArrayList;

public class Main {
  public static void main(String[] args) {
    String roman = "MCMLXXXII";
    Context context = new Context(roman);

    // Build the 'parse tree'
    ArrayList<Expression> tree = new ArrayList<>();
    tree.add(new ThousandExpression());
    tree.add(new HundredExpression());
    tree.add(new TenExpression());
    tree.add(new OneExpression());

    // Interpret
    for (Expression exp : tree) {
      exp.interpret(context);
    }

    System.out.println(roman + " = " + Integer.toString(context.getOutput()));
  }
}
