public class Runnable_ {

    public static void main(String[] args) {
        ThreadClass R1 = new ThreadClass("Thread-1");
        R1.start();
        ThreadClass R2 = new ThreadClass("Thread-2");
        R2.start();
    }   
}

class ThreadClass implements Runnable {
    private Thread t;
    private String name;

    ThreadClass(String name) {
        this.name = name;
        System.out.println("Creating " + name);
    }

    public void run() {
        System.out.println("Running " + name);
        try {
            for (int i = 4; i > 0; i--) {
                System.out.println(name + ": " + i);
                Thread.sleep(50);
            }
        } catch (InterruptedException e) {
            System.out.println(name + " interrupted");
        }
        System.out.println("Exiting " + name);
    }

    public void start () {
        System.out.println("Starting " + name);
        if (t == null) {
            t = new Thread(this, name);
            t.start();
        }
    }
}

