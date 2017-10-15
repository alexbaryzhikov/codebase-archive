public class ErrorLogger extends Logger {

  public ErrorLogger() {
    this.level = Logger.ERROR;
  }

  @Override
  protected boolean logMessage(Message message) {
    if (message.getLevel() >= this.level) {
      System.out.println("ErrorLogger: " + message.getMessage());
      return true;
    }
    return false;
  }
}
