package common;

import java.io.IOException;
import java.net.Socket;

public class Client implements Runnable {
    int clientNumber;
    int port;

    public Client(int clientNumber, int port) {
        this.clientNumber = clientNumber;
        this.port = port;
    }

    @Override
    public void run() {
        try {
            connectSendReceive(clientNumber);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    private void connectSendReceive(int i) throws IOException {
        System.out.println("Client " + i + ": connection");
        Socket socket = new Socket("localhost", port);
        System.out.println("Client " + i + ": sending message");
        MessageUtils.sendMessage(socket, Integer.toString(i));
        System.out.println("Client " + i + ": getting reply");
        MessageUtils.getMessage(socket);
        System.out.println("Client " + i + ": finished");
        socket.close();
    }
}
