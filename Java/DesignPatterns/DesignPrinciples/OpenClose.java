/* Software entities like classes, modules and functions should be open for extension
but closed for modifications. */

public class OpenClose {
    public static void main(String[] args) {
    }
}

/* ---------------------------------------------------------------------------- */
/* Open-Close Principle - Bad example
   - For each new shape added the unit testing of the GraphicEditor should be redone.
   - When a new type of shape is added the time for adding it will be high since the
     developer who add it should understand the logic of the GraphicEditor.
   - Adding a new shape might affect the existing functionality in an undesired way,
     even if the new shape works perfectly
*/

class GraphicEditor_ {
    public void drawShape(Shape_ s) {
        if (s.m_type == 1)
            drawRectangle((Rectangle_) s);
        else if (s.m_type == 2)
            drawCircle((Circle_) s);
    }
    public void drawCircle(Circle_ r) { /* .... */ }
    public void drawRectangle(Rectangle_ r) { /* .... */ }
}

class Shape_ {
    int m_type;
}

class Rectangle_ extends Shape_ {
    Rectangle_() {
        super.m_type = 1;
    }
}

class Circle_ extends Shape_ {
    Circle_() {
        super.m_type = 2;
    }
}

/* ---------------------------------------------------------------------------- */
/* Open-Close Principle - Good example
   - No unit testing required.
   - No need to understand the sourcecode from GraphicEditor.
   - Since the drawing code is moved to the concrete shape classes, it's a reduced
     risk to affect old functionallity when new functionallity is added.
*/

class GraphicEditor {
    public void drawShape(Shape s) {
        s.draw();
    }
}

abstract class Shape {
    abstract void draw();
}

class Rectangle extends Shape  {
    public void draw() {
        /* draw the rectangle */
    }
} 

class Circle extends Shape  {
    public void draw() {
        /* draw the circle */
    }
} 
