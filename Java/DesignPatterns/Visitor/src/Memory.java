public class Memory implements Component {

  private double sizeGB;

  public Memory(double sizeGB) {
    this.sizeGB = sizeGB;
  }

  public double getSizeGB() {
    return sizeGB;
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
