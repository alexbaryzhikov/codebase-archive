public class DebugFilter implements Filter {

  @Override
  public void apply(String request) {
    System.out.println("Logging request: " + request);
  }
}
