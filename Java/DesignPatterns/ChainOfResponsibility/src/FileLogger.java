public class FileLogger extends Logger {

  public FileLogger() {
    this.level = Logger.DEBUG;
  }

  @Override
  protected boolean logMessage(Message message) {
    if (message.getLevel() >= this.level) {
      System.out.println("FileLogger: " + message.getMessage());
      return true;
    }
    return false;
  }
}
