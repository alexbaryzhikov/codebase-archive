public class Cpu implements Component {

  private double frequencyGHz;

  public Cpu(double frequencyGHz) {
    this.frequencyGHz = frequencyGHz;
  }

  public double getFrequencyGHz() {
    return frequencyGHz;
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
