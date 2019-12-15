package threaded;

import threaded.scheduler.ExecutorClientScheduler;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;

public class Server implements Runnable {
    private final ConnectionManager connectionManager;
    private final ClientScheduler clientScheduler;
    private volatile boolean keepProcessing = true;

    public Server(int port, int millisecondsTimeout) throws IOException {
        ServerSocket serverSocket = new ServerSocket(port);
        serverSocket.setSoTimeout(millisecondsTimeout);
        connectionManager = new ConnectionManager(serverSocket);
        clientScheduler = new ExecutorClientScheduler(4);
    }

    public void run() {
        System.out.println("Server: starting");
        while (keepProcessing) {
            try {
                ClientConnection clientConnection = connectionManager.awaitClient();
                ClientRequestProcessor clientRequestProcessor =
                        new ClientRequestProcessor(clientConnection);
                clientScheduler.schedule(clientRequestProcessor);
            } catch (SocketException | SocketTimeoutException ignored) {
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    public void stopProcessing() {
        keepProcessing = false;
        connectionManager.finish();
    }
}