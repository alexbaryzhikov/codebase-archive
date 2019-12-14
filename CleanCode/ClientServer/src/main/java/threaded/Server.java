package threaded;

import common.MessageUtils;

import java.io.Closeable;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

public class Server implements Runnable {
    ServerSocket serverSocket;
    volatile boolean keepProcessing = true;

    public Server(int port, int millisecondsTimeout) throws IOException {
        serverSocket = new ServerSocket(port);
        serverSocket.setSoTimeout(millisecondsTimeout);
    }

    public void run() {
        System.out.println("Server: starting");
        while (keepProcessing) {
            try {
                System.out.println("Server: accepting client");
                Socket socket = serverSocket.accept();
                System.out.println("Server: got client");
                process(socket);
            } catch (Exception e) {
                handle(e);
            }
        }
    }

    private void process(Socket socket) {
        if (socket == null) return;

        Runnable clientHandler = () -> {
            try {
                System.out.println("Server: getting message");
                String message = MessageUtils.getMessage(socket);
                System.out.println("Server: got message: " + message);
                Thread.sleep(1000);
                System.out.println("Server: sending reply: " + message);
                MessageUtils.sendMessage(socket, "Processed: " + message);
                System.out.println("Server: sent");
                closeIgnoringException(socket);
            } catch (Exception e) {
                e.printStackTrace();
            }
        };

        Thread clientConnection = new Thread(clientHandler);
        clientConnection.start();
    }

    private void closeIgnoringException(Closeable socket) {
        if (socket != null)
            try {
                socket.close();
            } catch (IOException ignore) {
            }
    }

    private void handle(Exception e) {
        if (!(e instanceof SocketException)) {
            e.printStackTrace();
        }
    }

    public void stopProcessing() {
        keepProcessing = false;
        closeIgnoringException(serverSocket);
    }
}