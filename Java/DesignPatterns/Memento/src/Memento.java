public class Memento {

  // State
  private int firstNumber;
  private int secondNumber;

  public Memento(int firstNumber, int secondNumber) {
    this.firstNumber = firstNumber;
    this.secondNumber = secondNumber;
  }

  public int getFirstNumber() {
    return firstNumber;
  }

  public int getSecondNumber() {
    return secondNumber;
  }
}
