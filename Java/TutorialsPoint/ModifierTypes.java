public class ModifierTypes {
    public static void main(String[] args) {
        InstanceCounter.test();
    }
}

/* ---------------------------------------------------------------------------- */
/* Access modifiers */

/* Private */
class Logger {
    /* Directly accessible only from withing class */
    /* Is not inherited by subclasses */
    private String format;

    public String getFormat() {
      return this.format;
    }

    public void setFormat(String format) {
      this.format = format;
    }

    /* Public -- accessible from anywhere */
    public static void main(String[] arguments) {
    }
}

/* Protected */
class AudioPlayer {
    /* Accessible within this package and subclasses */
    protected boolean openSpeaker(Speaker sp) {
        // implementation details
        return false;
    }
}

class StreamingAudioPlayer {
    /* Overrides protected method */
    boolean openSpeaker(Speaker sp) {
        // implementation details
        return true;
    }
}

class Speaker {
}

/* ---------------------------------------------------------------------------- */
/* Non-access modifiers */

/* static */
class InstanceCounter {
    private static int numInstances = 0;

    InstanceCounter() {
        InstanceCounter.addInstance();
    }

    protected static int getCount() {
        return numInstances;
    }

    private static void addInstance() {
        numInstances++;
    }

    public static void test() {
        System.out.println("Starting with " + InstanceCounter.getCount() + " instances");
        for (int i = 0; i < 500; ++i)
            new InstanceCounter();
        System.out.println("Created " + InstanceCounter.getCount() + " instances");
    }
}

/* Abstract class can't be instantiated */
abstract class Caravan {
    private double price;
    private String model;
    private String year;
    public abstract void goFast();   // an abstract method
    public abstract void changeColor();
}

abstract class SuperClass {
    abstract void m();   // abstract method
}

class SubClass extends SuperClass {
    /* An instance variable is marked transient to indicate the JVM to skip
    the particular variable when serializing the object containing it. */
    public transient int limit = 55;   // will not persist
    public int b;   // will persist

    // implements the abstract method
    void m() {
    }

    /*  Synchronized method can be accessed by only one thread at a time */
    public synchronized void showDetails() {
    }
}

/* Final class can't be subclassed */
final class Final {
    final int value = 10;

    // The following are examples of declaring constants:
    public static final int BOXWIDTH = 6;
    static final String TITLE = "Manager";

    /* Final method can't be overriden */
    public final void changeName() {
        // body of method
    }

    public void changeValue() {
        value = 12;   // will give an error
    }
}

/* The volatile modifier is used to let the JVM know that a thread accessing
the variable must always merge its own private copy of the variable with the
master copy in the memory. */
class MyRunnable implements Runnable {
   private volatile boolean active;

   public void run() {
      active = true;
      while (active) {   // line 1
         // some code here
      }
   }

   public void stop() {
      active = false;   // line 2
   }
}
