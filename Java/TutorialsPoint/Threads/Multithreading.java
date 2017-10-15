import java.util.Random;

public class Multithreading {

    public static void main(String[] args) {
        Runnable ping = new DisplayMessage("ping");
        Thread thread1 = new Thread(ping);
        thread1.setDaemon(true);
        thread1.setName("ping_thread");
        System.out.println("Starting Ping thread");
        thread1.start();

        Runnable pong = new DisplayMessage("pong");
        Thread thread2 = new Thread(pong, "pong_thread");
        thread2.setPriority(Thread.MIN_PRIORITY);
        thread2.setDaemon(true);
        System.out.println("Starting Pong thread");
        thread2.start();

        Thread thread3 = new GuessANumber(27);
        System.out.println("Starting Guess27 thread");
        thread3.start();
        try {
            thread3.join();
        } catch (InterruptedException e) {
        }

        Thread thread4 = new GuessANumber(75);
        System.out.println("Starting Guess75 thread");
        thread4.start();
        System.out.println("Exiting main()");
    }
}

/* Create a thread to implement Runnable */
class DisplayMessage implements Runnable {
    private String message;

    public DisplayMessage(String message) {
        this.message = message;
    }

    public void run() {
        try {
            while (true) {
                System.out.println(message);
                Thread.sleep(1);
            }
        } catch (InterruptedException e) {
        }
    }
}

/* Create a thread to extend Thread */
class GuessANumber extends Thread {
    private int number;
    private Random rnd;

    public GuessANumber(int number) {
        super("Guess" + number + "_thread");
        this.number = number;
        rnd = new Random();
    }

    public void run() {
        int counter = 0;
        int guess = 0;
        do {
            guess = rnd.nextInt(100) + 1;
            System.out.println(this.getName() + " guesses " + guess);
            counter++;
        } while (guess != number);
        System.out.printf("%s correct in %d guesses\n", this.getName(), counter);
    }
}
