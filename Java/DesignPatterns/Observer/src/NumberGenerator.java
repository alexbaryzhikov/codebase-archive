import java.util.ArrayList;
import java.util.List;

public class NumberGenerator implements Observable {

  private List<Observer> observers = new ArrayList<>();
  private int state;

  @Override
  public void attach(Observer observer) {
    observers.add(observer);
  }

  @Override
  public void detach(Observer observer) {
    observers.remove(observer);
  }

  @Override
  public void notifyObservers() {
    for (Observer observer : observers) {
      observer.update();
    }
  }

  @Override
  public int getState() {
    return state;
  }

  @Override
  public void setState(int state) {
    this.state = state;
    notifyObservers();
  }
}
