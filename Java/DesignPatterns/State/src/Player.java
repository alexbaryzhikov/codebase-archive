public class Player implements Context {

  private State state;

  public Player() {
    state = null;
  }

  @Override
  public State getState() {
    return state;
  }

  @Override
  public void setState(State state) {
    this.state = state;
  }
}
