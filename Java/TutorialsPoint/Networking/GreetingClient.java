import java.net.*;
import java.io.*;

public class GreetingClient {

    public static void main(String[] args) {
        String serverName = args[0];
        int port = Integer.parseInt(args[1]);
        
        try {
            /* Create socket */
            System.out.printf("Connecting to %s on port %d...\n", serverName, port);
            Socket client = new Socket(serverName, port);

            /* Establish data streams */
            System.out.println("Just connected to " + client.getRemoteSocketAddress());
            OutputStream outToServer = client.getOutputStream();
            InputStream inFromServer = client.getInputStream();
            DataOutputStream out = new DataOutputStream(outToServer);
            DataInputStream in = new DataInputStream(inFromServer);

            /* Send/receive message and close socket */
            out.writeUTF("Hello from " + client.getLocalSocketAddress());
            System.out.println("Server: " + in.readUTF());
            client.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
