public class StaticNestedClass {
    static class NestedClass {
        public void myMethod() {
            System.out.println("This is a static nested class");
        }
    }

    public static void main(String[] args) {
        StaticNestedClass.NestedClass nested = new StaticNestedClass.NestedClass();    
        nested.myMethod();
    }
}
