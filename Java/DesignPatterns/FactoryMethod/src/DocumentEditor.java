import java.util.ArrayList;

/**
 * Abstract Factory
 */
public abstract class DocumentEditor {

  private ArrayList<Document> documents;

  DocumentEditor() {
    documents = new ArrayList<>();
  }

  public ArrayList<Document> getDocuments() {
    return documents;
  }

  /**
   * Factory method
   */
  public abstract Document create(int typeId, String name);

  public abstract Document newDocument(int typeId, String name);

  public abstract Document openDocument(String name);

  public abstract void saveDocument(Document document);

  public abstract void closeDocument(Document document);
}
