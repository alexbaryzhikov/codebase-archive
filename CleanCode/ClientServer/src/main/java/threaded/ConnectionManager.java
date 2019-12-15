package threaded;

import common.ObjectUtils;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

public class ConnectionManager {
    private final ServerSocket serverSocket;

    public ConnectionManager(ServerSocket serverSocket) {
        this.serverSocket = serverSocket;
    }

    public ClientConnection awaitClient() throws IOException {
        System.out.println("Server: accepting client");
        Socket socket = serverSocket.accept();
        System.out.println("Server: got client");
        return new ClientConnection(socket);
    }

    public void finish() {
        ObjectUtils.closeIgnoringException(serverSocket);
    }
}