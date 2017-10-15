public class Hdd implements Component {

  private double sizeTB;

  public Hdd(double sizeTB) {
    this.sizeTB = sizeTB;
  }

  public double getSizeTB() {
    return sizeTB;
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
