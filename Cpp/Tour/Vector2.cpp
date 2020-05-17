#include <iostream>
#include <sstream>
#include <string>

class Vector {
    const std::string tag;
    int size;
    double *elements;

   public:
    explicit Vector(std::string tag) : tag{tag}, size{0}, elements{nullptr} {
        std::cout << "constr of " << tag << std::endl;
    }

    Vector(std::string tag, int size) : tag{tag}, size{size}, elements{new double[size]} {
        std::cout << "constr of " << tag << std::endl;
        for (int i = 0; i < size; i++) elements[i] = 0;
    }

    Vector(const Vector &other)
        : tag{other.tag}, size{other.size}, elements{new double[other.size]} {
        std::cout << "constr copy of " << other.tag << std::endl;
        for (int i = 0; i < other.size; i++) elements[i] = other[i];
    }

    Vector(std::string tag, const Vector &other)
        : tag{tag}, size{other.size}, elements{new double[other.size]} {
        std::cout << "constr copy from " << other.tag << " to " << tag << std::endl;
        for (int i = 0; i < other.size; i++) elements[i] = other[i];
    }

    Vector(Vector &&other) : tag{other.tag}, size{other.size}, elements{other.elements} {
        std::cout << "constr move of " << other.tag << std::endl;
        other.size = 0;
        other.elements = nullptr;
    }

    Vector(std::string tag, Vector &&other) : tag{tag}, size{other.size}, elements{other.elements} {
        std::cout << "constr move from " << other.tag << " to " << tag << std::endl;
        other.size = 0;
        other.elements = nullptr;
    }

    ~Vector() { delete[] elements; }

    std::string get_tag() const { return tag; }

    int get_size() const { return size; }

    double &operator[](int i) { return elements[i]; }

    const double &operator[](int i) const { return elements[i]; }

    Vector &operator=(const Vector &other) {
        std::cout << "assign copy from " << other.tag << " to " << tag << std::endl;
        delete[] elements;
        size = other.size;
        elements = new double[other.size];
        for (int i = 0; i < size; i++) elements[i] = other[i];
        return *this;
    }

    Vector &operator=(Vector &&other) {
        std::cout << "assign move from " << other.tag << " to " << tag << std::endl;
        delete[] elements;
        size = other.size;
        elements = other.elements;
        other.size = 0;
        other.elements = nullptr;
        return *this;
    }

    std::string to_string() const {
        std::stringstream ss;
        ss << tag << ": [";
        for (int i = 0; i < size; i++) {
            ss << elements[i];
            if (i < size - 1) ss << ", ";
        }
        ss << "]";
        return ss.str();
    }
};

Vector operator+(const Vector &a, const Vector &b) {
    Vector result{"result", a.get_size()};
    for (int i = 0; i < a.get_size(); i++) result[i] = a[i] + b[i];
    return result;
}

int main() {
    Vector v1{"v1", 3};
    Vector v2{"v2", v1};
    Vector v3{"v3"};
    v3 = v1;

    v1[0] = 1;
    v2[1] = 1;
    v3[2] = 1;

    std::cout << v1.to_string() << std::endl;
    std::cout << v2.to_string() << std::endl;
    std::cout << v3.to_string() << std::endl;

    Vector v4{"v4"};
    v4 = v1 + v2;
    std::cout << v4.to_string() << std::endl;

    Vector v5{"v5", v2 + v3};
    std::cout << v5.to_string() << std::endl;

    Vector v6 = std::move(v4 + v5);
    std::cout << v6.to_string() << std::endl;
}
