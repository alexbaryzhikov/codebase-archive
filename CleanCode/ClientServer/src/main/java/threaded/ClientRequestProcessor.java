package threaded;

import java.io.IOException;

public class ClientRequestProcessor {
    private final ClientConnection clientConnection;

    public ClientRequestProcessor(ClientConnection clientConnection) {
        this.clientConnection = clientConnection;
    }

    public void process() {
        try {
            System.out.println("Server: getting message");
            String message = clientConnection.getMessage();
            System.out.println("Server: got message: " + message);

            Thread.sleep(1000);

            System.out.println("Server: sending reply: " + message);
            clientConnection.sendMessage("Processed: " + message);
            System.out.println("Server: sent");

            clientConnection.close();
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}