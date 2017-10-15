public abstract class Expression {

  private String one;
  private String four;
  private String five;
  private String nine;
  private int multiplier;

  public Expression(String one, String four, String five, String nine, int multiplier) {
    this.one = one;
    this.four = four;
    this.five = five;
    this.nine = nine;
    this.multiplier = multiplier;
  }

  public void interpret(Context context) {
    if (context.getInput().length() == 0) {
      return;
    }

    if (context.getInput().startsWith(nine)) {
      context.setOutput(context.getOutput() + (9 * multiplier));
      context.setInput(context.getInput().substring(2));
    } else if (context.getInput().startsWith(four)) {
      context.setOutput(context.getOutput() + (4 * multiplier));
      context.setInput(context.getInput().substring(2));
    } else if (context.getInput().startsWith(five)) {
      context.setOutput(context.getOutput() + (5 * multiplier));
      context.setInput(context.getInput().substring(1));
    }

    while (context.getInput().startsWith(one)) {
      context.setOutput(context.getOutput() + multiplier);
      context.setInput(context.getInput().substring(1));
    }
  }
}
