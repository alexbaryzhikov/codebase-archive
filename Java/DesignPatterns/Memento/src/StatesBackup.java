import java.util.ArrayDeque;
import java.util.Deque;
import java.util.NoSuchElementException;

public class StatesBackup implements CareTaker {

  private Deque<Memento> mementos = new ArrayDeque<>();

  @Override
  public void push(Memento memento) {
    mementos.push(memento);
  }

  @Override
  public Memento pop() {
    try {
      return mementos.pop();
    } catch (NoSuchElementException e) {
      System.out.println("Keeping state: nothing to undo");
    }
    return null;
  }
}
