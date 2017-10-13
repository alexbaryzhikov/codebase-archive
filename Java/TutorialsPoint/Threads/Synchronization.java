public class Synchronization {
    public static void main(String[] args) {
        Print p = new Print();
        ThreadClass3 T1 = new ThreadClass3("Thread_1", p);
        ThreadClass3 T2 = new ThreadClass3("Thread_2", p);

        T1.start();
        T2.start();
        try {
            T1.join();
            T2.join();
        } catch (InterruptedException e) {
        }
    }
}

class Print {
    public void count() {
        for (int i = 5; i > 0; i--)
            System.out.println("Counter: " + i);
    }
}

class ThreadClass3 extends Thread {
    Print print;

    ThreadClass3(String name, Print print) {
        super(name);
        this.print = print;
    }

    public void run() {
        /* Lock print and run count() */
        synchronized (print) {
            print.count();
        }
        System.out.println("Exiting " + this.getName());
    }

    public void start() {
        System.out.println("Starting " + this.getName());
        super.start();
    }
}
