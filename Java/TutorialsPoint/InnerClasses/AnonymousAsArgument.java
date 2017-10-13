// interface
interface Message {
    String greet();
}

public class AnonymousAsArgument {
    // method which accepts the object of interface Message
    public void displayMessage(Message m) {
        System.out.println(m.greet());
        System.out.println("This is an example of anonymous inner class as an argument");  
    }

    public static void main(String[] args) {
        AnonymousAsArgument obj = new AnonymousAsArgument();
        // Passing an anonymous inner class as an argument
        obj.displayMessage(new Message() {
            public String greet() {
                return "Hello";
            }
        });
    }
}
