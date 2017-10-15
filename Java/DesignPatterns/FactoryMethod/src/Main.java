/* --------------------------------------------------------
 * Factory Method
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Defines an interface for creating objects, but let subclasses to decide which class to instantiate
 * - Refers to the newly created object through a common interface
 *
 * Applicability
 *
 * - Class can't anticipate the type of objects it is supposed to create
 * - Class wants its subclasses to specify the type of a newly created object
 *
 * Examples
 *
 * - Document editor
 */

public class Main {

  public static void main(String[] args) {
    // Get document editor instance
    DocumentEditor editor = new MyAwesomeEditor();

    // Create new documents
    Document foo = editor.newDocument(Document.TXT, "foo");
    Document bar = editor.newDocument(Document.HTML, "bar");

    // Open existing document
    Document baz = editor.openDocument("baz.pdf");

    // Editing documents...

    // Save documents
    editor.saveDocument(foo);
    editor.saveDocument(bar);
    editor.saveDocument(baz);

    // Close all documents
    editor.closeDocument(foo);
    editor.closeDocument(bar);
    editor.closeDocument(baz);
  }
}
