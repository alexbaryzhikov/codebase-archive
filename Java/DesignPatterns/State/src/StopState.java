public class StopState implements State {

  @Override
  public void apply(Context player) {
    player.setState(this);
  }

  public String toString() {
    return "Stop state";
  }
}
