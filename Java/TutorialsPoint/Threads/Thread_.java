public class Thread_ {

    public static void main(String[] args) {
        ThreadClass2 T1 = new ThreadClass2("Thread-1");
        T1.start();
        ThreadClass2 T2 = new ThreadClass2("Thread-2");
        T2.start();
    }   
}

class ThreadClass2 extends Thread {
    ThreadClass2(String name) {
        super(name);
        System.out.println("Creating " + name);
    }

    public void run() {
        System.out.println("Running " + this.getName());
        try {
            for (int i = 4; i > 0; i--) {
                System.out.println(this.getName() + ": " + i);
                Thread.sleep(50);
            }
        } catch (InterruptedException e) {
            System.out.println(this.getName() + " interrupted");
        }
        System.out.println("Exiting " + this.getName());
    }

    public void start() {
        System.out.println("Starting " + this.getName());
        super.start();
    }
}

