public class Motherboard implements Component {

  public String getStatus() {
    return "OK";
  }

  @Override
  public void accept(Visitor visitor) {
    visitor.visit(this);
  }
}
