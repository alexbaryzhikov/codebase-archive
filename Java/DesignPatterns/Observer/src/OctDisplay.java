public class OctDisplay implements Observer {

  private Observable observable;

  public OctDisplay(Observable observable) {
    this.observable = observable;
    this.observable.attach(this);
  }

  @Override
  public void update() {
    System.out.println("Octal string: " + Integer.toOctalString(observable.getState()));
  }
}
