public class Calculator implements Originator {

  private int firstNumber;
  private int secondNumber;

  @Override
  public Memento createStateBackup() {
    return new Memento(firstNumber, secondNumber);
  }

  @Override
  public boolean restoreState(Memento memento) {
    if (memento != null) {
      this.firstNumber = memento.getFirstNumber();
      this.secondNumber = memento.getSecondNumber();
      return true;
    }
    return false;
  }

  @Override
  public void setFirstNumber(int number) {
    this.firstNumber = number;
  }

  @Override
  public void setSecondNumber(int number) {
    this.secondNumber = number;
  }

  @Override
  public int getResult() {
    return firstNumber + secondNumber;
  }
}
