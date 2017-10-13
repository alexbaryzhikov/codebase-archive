import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.util.Random;

public class PingPongServer extends Thread {
    private ServerSocket serverSocket;

    public PingPongServer(int port) throws IOException {
        serverSocket = new ServerSocket(port);
        serverSocket.setSoTimeout(50000);
    }

    public void run() {
        DataInputStream in;
        DataOutputStream out;
        final byte EXIT = 99;
        final byte PING = 3;
        final byte PONG = 11;
        int port = serverSocket.getLocalPort();
        Random rnd = new Random();
        Socket server;
        String reply;
        String rot = (char)8213 + "\\|/";

        try {
            /* Establish connection */
            System.out.printf("Waiting for player on port %d...\n", port);
            server = serverSocket.accept();
            System.out.println("Сonnected to " + server.getRemoteSocketAddress());
            in = new DataInputStream(server.getInputStream());
            out = new DataOutputStream(server.getOutputStream());

            /* Play ping-pong until client quits */
            while (true) {
                System.out.print("server>  ");
                for (int i = 0; i < rnd.nextInt(20)+5; i++) {
                    System.out.print("\b" + rot.charAt(i%4));
                    Thread.sleep(100);
                }
                out.write(PING);
                System.out.println("\bping!");
                if (in.readByte() != EXIT)
                    System.out.println("player> pong!");
                else
                    break;
            }
            System.out.println("Player has left");
            server.close();

        } catch (InterruptedException e) {
            System.out.println("Game interrupted!");
        } catch (SocketTimeoutException e) {
            System.out.println("Socket timed out!");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: PingPongServer <port>");
            System.exit(0);
        }
        int port = Integer.parseInt(args[0]);
        try {
            Thread t = new PingPongServer(port);
            t.start();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
