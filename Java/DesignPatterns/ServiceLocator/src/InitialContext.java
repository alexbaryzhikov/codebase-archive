public class InitialContext {

  public Object lookup(String serviceName) {
    if (serviceName.equalsIgnoreCase("service1")) {
      System.out.println("Loking up and creating a new Service1 object");
      return new Service1();
    } else if (serviceName.equalsIgnoreCase("service2")) {
      System.out.println("Loking up and creating a new Service2 object");
      return new Service2();
    } else {
      throw new IllegalArgumentException("Unknown service: " + serviceName);
    }
  }
}
