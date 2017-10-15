public class DefensiveStrategy implements Strategy {

  @Override
  public int moveCommand() {
    System.out.println("Defensive::moveCommand");
    return -1;
  }
}
