public class AggressiveStrategy implements Strategy {

  @Override
  public int moveCommand() {
    System.out.println("Aggressive::moveCommand");
    return 1;
  }
}
