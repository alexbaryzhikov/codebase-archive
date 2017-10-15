public class NormalStrategy implements Strategy {

  @Override
  public int moveCommand() {
    System.out.println("Normal::moveCommand");
    return 0;
  }
}
