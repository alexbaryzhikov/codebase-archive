public class TxtDocument implements Document {

  private String name;

  TxtDocument(String name) {
    if (!name.endsWith(".txt")) {
      name = name + ".txt";
    }
    this.name = name;
    System.out.println(name + " created");
  }

  @Override
  public void open() {
    System.out.println(name + " opened");
  }

  @Override
  public void save() {
    System.out.println(name + " saved");
  }

  @Override
  public void close() {
    System.out.println(name + " closed");
  }
}
