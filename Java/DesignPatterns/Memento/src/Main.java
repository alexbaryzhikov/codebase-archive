/* --------------------------------------------------------
 * Memento
 * --------------------------------------------------------
 *
 * Intent
 * - Capture the internal state of an object without violating encapsulation and thus providing
 *   means for restoring the object into initial state
 *
 * This simple example is a calculator that finds the result of addition of two numbers, with
 * the additional option to undo last operation and restore previous result
 */

public class Main {

  private static Calculator calculator = new Calculator();
  private static StatesBackup backup = new StatesBackup();


  public static void main(String[] args) {
    calculateAndSave(10, 20, true);
    calculateAndSave(100, -30, true);
    calculateAndSave(1000, 450, false);

    undoOperation();
    undoOperation();
    undoOperation();
  }

  private static void calculateAndSave(int firstNumber, int secondNumber, boolean doSave) {
    // Enter numbers
    calculator.setFirstNumber(firstNumber);
    calculator.setSecondNumber(secondNumber);

    // Store result
    if (doSave) {
      backup.push(calculator.createStateBackup());
    }

    // Print result
    System.out.println(calculator.getResult());

  }

  private static void undoOperation() {
    if (calculator.restoreState(backup.pop())) {
      System.out.println(calculator.getResult());
    }
  }
}
