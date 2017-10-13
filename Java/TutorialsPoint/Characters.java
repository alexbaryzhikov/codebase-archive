public class Characters {
    public static void main(String[] args) {
        char c = 'a';

        // Unicode for uppercase Greek omega character
        char uniChar = '\u039A';

        // an array of chars
        char[] charArray ={ 'a', 'b', 'c', 'd', 'e' };

        // Here following primitive char 'a'
        // is boxed into the Character object ch
        Character ch = 'a';

        // Here primitive 'x' is boxed for method test,
        // return is unboxed to char 'c'
        c = test('x');

        System.out.println("She said \"Hello!\" to me.");
    }

    static char test(Character c) {
        return c;
    }
}
