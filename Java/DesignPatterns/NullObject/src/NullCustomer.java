public class NullCustomer extends Customer {

  @Override
  public boolean isNull() {
    return true;
  }

  @Override
  public String getName() {
    return "<Not available>";
  }
}
