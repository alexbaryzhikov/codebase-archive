public class AuthenticationFilter implements Filter {

  @Override
  public void apply(String request) {
    System.out.println("Authenticating request: " + request);
  }
}
