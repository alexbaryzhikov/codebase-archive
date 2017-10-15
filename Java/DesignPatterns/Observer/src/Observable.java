public interface Observable {
  void attach(Observer observer);
  void detach(Observer observer);
  void notifyObservers();
  int getState();
  void setState(int state);
}
