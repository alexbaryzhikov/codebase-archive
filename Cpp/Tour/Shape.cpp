#include <iostream>
#include <sstream>
#include <vector>

struct Point {
    int x;
    int y;
};

std::string to_string(Point point) {
    std::stringstream ss;
    ss << "(" << point.x << ", " << point.y << ")";
    return ss.str();
}

class Shape {
   public:
    virtual Point center() const = 0;
    virtual void move(Point to) = 0;
    virtual void draw() const = 0;
    virtual void rotate(int angle) = 0;
    virtual ~Shape() {}
};

class Circle : public Shape {
    Point c;
    int r;

   public:
    Circle(Point center, int radius) : c{center}, r{radius} {};

    Point center() const override { return c; }
    void move(Point to) override { c = to; }
    void draw() const override { std::cout << "Draw circle at " << to_string(c) << "\n"; };
    void rotate(int) override {}
};

class Arc : public Circle {
    int angSt;
    int angEn;

   public:
    Arc(Point center, int radius, int andgleStart, int angleEnd)
        : Circle{center, radius}, angSt{andgleStart}, angEn{angleEnd} {}

    void draw() const override { std::cout << "Draw arc at " << to_string(center()) << "\n"; }
    void rotate(int angle) {
        std::cout << "Rotate arc by " << angle << "\n";
        angSt += angle;
        angEn += angle;
    }
};

class Smiley : public Circle {
    std::vector<Shape *> eyes;
    Shape *mouth;

   public:
    Smiley(Point center, int radius) : Circle{center, radius}, mouth{nullptr} {}

    ~Smiley() {
        delete mouth;
        for (auto eye : eyes) delete eye;
    }

    void draw() const override;
    void rotate(int angle) override { std::cout << "Rotate smiley by " << angle << "\n"; };
    void add_eye(Shape *s) { eyes.push_back(s); }
    void set_mouth(Shape *s) { mouth = s; };
};

void Smiley::draw() const {
    Circle::draw();
    for (auto eye : eyes) eye->draw();
    mouth->draw();
}

void rotate_all(std::vector<Shape *> &shapes, int angle) {
    for (auto shape : shapes) shape->rotate(angle);
}

int main() {
    Smiley *smiley = new Smiley{Point{0, 0}, 10};
    smiley->add_eye(new Circle{Point{-1, 1}, 1});
    smiley->add_eye(new Circle{Point{1, 1}, 1});
    smiley->set_mouth(new Arc{Point{0, -1}, 3, 0, 180});

    smiley->draw();
    smiley->rotate(123);
    std::cout << "Smiley is at " << to_string(smiley->center()) << std::endl;
    smiley->move(Point{1, 1});
    std::cout << "Smiley is at " << to_string(smiley->center()) << std::endl;
}
