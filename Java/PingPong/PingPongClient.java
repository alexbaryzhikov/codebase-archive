import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.IOException;
import java.net.Socket;
import java.util.Random;

public class PingPongClient extends Thread {
    String host;
    int port;

    public PingPongClient(String host, int port) {
        this.host = host;
        this.port = port;
    }

    public void run() {
        DataInputStream in;
        DataOutputStream out;
        final byte EXIT = 99;
        final byte PING = 3;
        final byte PONG = 11;
        Random rnd = new Random();
        Socket client;
        String rot = (char)8213 + "\\|/";

        try {
            /* Establish connection */
            System.out.printf("Connecting to %s on port %d... ", host, port);
            client = new Socket(host, port);
            System.out.println("success");
            in = new DataInputStream(client.getInputStream());
            out = new DataOutputStream(client.getOutputStream());

            /* Play ping-pong for some time */
            for (int i = 0; i < 5; i++) {
                if (in.readByte() == PING) {
                    System.out.println("server> ping!");
                    System.out.print("player>  ");
                    for (int j = 0; j < rnd.nextInt(20)+5; j++) {
                        System.out.print("\b" + rot.charAt(j%4));
                        Thread.sleep(100);
                    }
                    out.write(PONG);
                    System.out.println("\bpong!");
                } else {
                    System.out.println("oops :)");
                    break;
                }
            }
            System.out.println("Game finished");
            out.write(EXIT);
            client.close();

        } catch (InterruptedException s) {
            System.out.println("Game interrupted!");
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static void main(String[] args) {
        if (args.length == 0) {
            System.out.println("Usage: PingPongClient <host> <port>");
            System.exit(0);
        }
        String host = args[0];
        int port = Integer.parseInt(args[1]);
        Thread t = new PingPongClient(host, port);
        t.start();
    }
}
