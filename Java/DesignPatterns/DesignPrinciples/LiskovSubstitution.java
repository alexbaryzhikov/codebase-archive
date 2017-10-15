/* Derived types must be completely substitutable for their base types. */

public class LiskovSubstitution {
    private static Rectangle getNewRectangle() {
        /* It can be an object returned by some factory ... */
        return new Square();
    }

    public static void main(String[] args) {
        Rectangle r = getNewRectangle();
        
        /* User knows that r is a Rectangle. */
        /* He assumes he's able to set the width and height normally. */
        r.setWidth(5);
        r.setHeight(10);

        /* Now he's surprised to see that the area is 100 instead of 50. */
        System.out.println(r.getArea());
    }
}

/* ---------------------------------------------------------------------------- */
/* Violation of Likov's Substitution Principle */

class Rectangle {
    protected int m_width;
    protected int m_height;

    public void setWidth(int width) {
        m_width = width;
    }

    public void setHeight(int height) {
        m_height = height;
    }

    public int getWidth() {
        return m_width;
    }

    public int getHeight() {
        return m_height;
    }

    public int getArea() {
        return m_width * m_height;
    }
}

class Square extends Rectangle {
    public void setWidth(int width) {
        m_width = width;
        m_height = width;
    }

    public void setHeight(int height) {
        m_width = height;
        m_height = height;
    }
}
