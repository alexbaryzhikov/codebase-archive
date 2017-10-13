public class MethodAsArg {

    public interface Lambda {
        void apply();
    }

    static void method(Lambda foo) {
        foo.apply();
    }

    public static void main(String[] args) {
        method(() -> System.out.println("hi"));
    }
}
