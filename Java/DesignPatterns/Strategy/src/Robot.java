public class Robot {

  private Strategy strategy;
  private String name;

  public Robot(String name) {
    this.name = name;
  }

  public Strategy getStrategy() {
    return strategy;
  }

  public void setStrategy(Strategy strategy) {
    this.strategy = strategy;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public void move() {
    System.out.println(name + "'s next move:");
    int command = strategy.moveCommand();
    System.out.println("Command code: " + command);
  }
}
