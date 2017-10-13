class OuterClass {
    int num = 42;

    /* inner class */
    private class InnerClass {
        public void print() {
            System.out.println("This is an inner class");
        }
    }

    /* access method for the inner class */
    void printInner() {
        InnerClass inner = new InnerClass();
        inner.print();
    }
}

public class InnerClassTest {
    public static void main(String[] args) {
        OuterClass outer = new OuterClass();
        outer.printInner();
    }
}
