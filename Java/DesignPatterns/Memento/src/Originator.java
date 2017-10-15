public interface Originator {

  // Memento operations
  Memento createStateBackup();
  boolean restoreState(Memento memento);

  // User services
  void setFirstNumber(int number);
  void setSecondNumber(int number);
  int getResult();
}
