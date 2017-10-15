public class HexDisplay implements Observer {

  private Observable observable;

  public HexDisplay(Observable observable) {
    this.observable = observable;
    this.observable.attach(this);
  }

  @Override
  public void update() {
    System.out.println("Hex string: " + Integer.toHexString(observable.getState()));
  }
}
