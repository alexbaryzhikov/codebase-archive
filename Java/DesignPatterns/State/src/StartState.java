public class StartState implements State {

  @Override
  public void apply(Context player) {
    player.setState(this);
  }

  public String toString() {
    return "Start state";
  }
}
