public class ConsoleLogger extends Logger {

  public ConsoleLogger() {
    this.level = Logger.INFO;
  }

  @Override
  protected boolean logMessage(Message message) {
    if (message.getLevel() >= this.level) {
      System.out.println("ConsoleLogger: " + message.getMessage());
      return true;
    }
    return false;
  }
}
