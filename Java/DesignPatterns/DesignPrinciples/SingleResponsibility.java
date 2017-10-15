/* A class should have only one reason to change. */

public class SingleResponsibility {
    public static void main(String[] args) {
    }
}

/* ---------------------------------------------------------------------------- */
/* Single Responsibility Principle - Bad example
   - Adding a new protocol will create the need to add code for parsing and serializing
     the content for each type of field.
   - Adding a new content type (like html) make us to add code for each protocol implemented.
*/

interface IEmail_ {
    public void setSender(String sender);
    public void setReceiver(String receiver);
    public void setContent(String content);
}

class Email_ implements IEmail_ {
    public void setSender(String sender) { /* .... */ }
    public void setReceiver(String receiver) { /* .... */ }
    public void setContent(String content) { /* .... */ }
}

/* ---------------------------------------------------------------------------- */
/* Single Responsibility Principle - Good example
   - Adding a new protocol causes changes only in the Email class.
   - Adding a new type of content supported causes changes only in Content class.
*/

interface IEmail {
    public void setSender(String sender);
    public void setReceiver(String receiver);
    public void setContent(IContent content);
}

interface IContent {
    public String getAsString(); /* used for serialization */
}

class Content implements IContent {
    public String getAsString() {
        /* .... */
        return "Content string";
    }
}

class Email implements IEmail {
    public void setSender(String sender) { /* .... */ }
    public void setReceiver(String receiver) { /* .... */ }
    public void setContent(IContent content) { /* .... */ }
}
