public class MethodLocalClass {
    /* instance method of the outer class */
    void myMethod() {
        int num = 23;

        // method-local inner class
        class InnerClass {
            public void print() {
                System.out.println("This is method inner class " + num);     
            }   
        }

        // Accessing the inner class
        InnerClass inner = new InnerClass();
        inner.print();
    }

    public static void main(String[] args) {
        MethodLocalClass outer = new MethodLocalClass();
        outer.myMethod();           
    }
}
