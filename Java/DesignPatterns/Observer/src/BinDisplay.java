public class BinDisplay implements Observer {

  private Observable observable;

  public BinDisplay(Observable observable) {
    this.observable = observable;
    this.observable.attach(this);
  }

  @Override
  public void update() {
    System.out.println("Binary string: " + Integer.toBinaryString(observable.getState()));
  }
}
