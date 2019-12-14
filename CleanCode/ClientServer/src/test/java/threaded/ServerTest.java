package threaded;

import common.Client;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class ServerTest {
    private static final int PORT = 18009;
    private static final int TIMEOUT = 2000;

    Server server;
    Thread serverThread;

    @Before
    public void createServer() throws Exception {
        try {
            server = new Server(PORT, TIMEOUT);
            serverThread = new Thread(server);
            serverThread.start();
        } catch (Exception e) {
            e.printStackTrace();
            throw e;
        }
    }

    @After
    public void shutdownServer() throws InterruptedException {
        if (server != null) {
            server.stopProcessing();
            serverThread.join();
        }
    }

    @Test(timeout = 5000)
    public void shouldRunInUnderFiveSeconds() throws Exception {
        Thread[] threads = new Thread[10];

        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(new Client(i, PORT));
            threads[i].start();
        }

        for (Thread thread : threads) {
            thread.join();
        }
    }
}