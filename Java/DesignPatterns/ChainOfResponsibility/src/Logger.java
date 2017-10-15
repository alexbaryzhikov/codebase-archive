public abstract class Logger {

  public static int INFO = 0;
  public static int DEBUG = 1;
  public static int ERROR = 2;

  protected int level;

  // Next element in chain
  private Logger nextLogger;

  public void setNextLogger(Logger nextLogger) {
    this.nextLogger = nextLogger;
  }

  protected abstract boolean logMessage(Message message);

  public void onMessageReceive(Message message) {
    // Attempt to handle message
    boolean handledByThis = logMessage(message);
    // Push message down the chain
    if (!handledByThis) {
      if (nextLogger != null) {
        nextLogger.onMessageReceive(message);
      } else {
        System.out.println("No logger for message: " + message.getLevel() + ", "
            + message.getMessage());
      }
    }
  }
}
