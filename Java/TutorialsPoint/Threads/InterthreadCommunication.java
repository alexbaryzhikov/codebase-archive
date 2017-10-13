class Chat {
    boolean flag = false;

    public synchronized void Question(String from, String msg) {
        if (flag) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        System.out.println(from + ":\n    " + msg);
        flag = true;
        notify();
    }

    public synchronized void Answer(String from, String msg) {
        if (!flag) {
            try {
                wait();
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
        System.out.println(from + ":\n    " + msg);
        flag = false;
        notify();
    }
}

class T1 implements Runnable {
    Chat m;
    Thread t;
    String[] s = { "Hi, Bob.", "How are you?", "I am also doing fine!" };

    public T1(Chat m) {
        this.m = m;
        t = new Thread(this, "Alice");
        t.start();
    }

    public void run() {
        for (int i = 0; i < s.length; i++) {
            m.Question(t.getName(), s[i]);
        }
    }
}

class T2 implements Runnable {
    Chat m;
    Thread t;
    String[] s = { "Hi, Alice.", "I am good, what about you?", "Great!" };

    public T2(Chat m) {
        this.m = m;
        t = new Thread(this, "Bob");
        t.start();
    }

    public void run() {
        for (int i = 0; i < s.length; i++) {
            m.Answer(t.getName(), s[i]);
        }
    }
}

public class InterthreadCommunication {
    public static void main(String[] args) {
        Chat m = new Chat();
        new T1(m);
        new T2(m);
    }
}
