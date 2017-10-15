public class HtmlDocument implements Document {

  private String name;

  HtmlDocument(String name) {
    if (!name.endsWith(".html")) {
      name = name + ".html";
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
