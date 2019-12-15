package threaded;

import common.MessageUtils;
import common.ObjectUtils;

import java.io.IOException;
import java.net.Socket;

public class ClientConnection {
    private final Socket socket;

    public ClientConnection(Socket socket) {
        this.socket = socket;
    }

    public String getMessage() throws IOException {
        return MessageUtils.getMessage(socket);
    }

    public void sendMessage(String message) throws IOException {
        MessageUtils.sendMessage(socket, message);
    }

    public void close() {
        ObjectUtils.closeIgnoringException(socket);
    }
}