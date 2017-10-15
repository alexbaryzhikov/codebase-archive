public class Message {

  private int level;
  private String message;

  public Message(int level, String message) {
    this.level = level;
    this.message = message;
  }

  public int getLevel() {
    return level;
  }

  public String getMessage() {
    return message;
  }
}
