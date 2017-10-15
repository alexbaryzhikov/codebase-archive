public class CustomerFactory {

  private static final String[] names = {"Rob", "Joe", "Julie"};

  public static Customer create(String name) {
    for (String n : names) {
      if (n.equalsIgnoreCase(name)) {
        return new RealCustomer(name);
      }
    }
    return new NullCustomer();
  }
}
