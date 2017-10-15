/**
 * Concrete factory
 */
public class MyAwesomeEditor extends DocumentEditor {

  MyAwesomeEditor() {
    super();
  }

  /**
   * Factory method
   */
  @Override
  public Document create(int typeId, String name) {
    switch (typeId) {
      case Document.TXT:
        return new TxtDocument(name);
      case Document.HTML:
        return new HtmlDocument(name);
      case Document.PDF:
        return new PdfDocument(name);
      default:
        throw new IllegalArgumentException("Unrecognized type ID: " + typeId);
    }
  }

  @Override
  public Document newDocument(int typeId, String name) {
    Document document = create(typeId, name);
    getDocuments().add(document);
    return document;
  }

  @Override
  public Document openDocument(String name) {
    int typeId;
    if (name.endsWith(".html")) {
      typeId = Document.HTML;
    } else if (name.endsWith((".pdf"))) {
      typeId = Document.PDF;
    } else {
      typeId = Document.TXT;
    }
    Document document = create(typeId, name);
    getDocuments().add(document);
    document.open();
    return document;
  }

  @Override
  public void saveDocument(Document document) {
    document.save();
  }

  @Override
  public void closeDocument(Document document) {
    getDocuments().remove(document);
    document.close();
  }
}
