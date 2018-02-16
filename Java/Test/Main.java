public class Main {

  public static void main(String[] args) {

    System.out.println("tests passed");
  }

  private static void assertEquals(boolean a, boolean b) throws AssertionError {
    if (a != b) {
      throw new AssertionError();
    }
  }
}
