#include <iostream>
#include <sstream>
#include <string>

template <typename T>
class Vector {
    size_t sz;
    T *elements;

   public:
    explicit Vector(size_t n) : sz{n}, elements{new T[n]} {
        for (size_t i = 0; i < n; i++) elements[i] = nullptr;
    }

    Vector(std::initializer_list<T> l) : sz{l.size()}, elements{new T[l.size()]} {
        for (std::size_t i = 0; i < l.size(); i++) elements[i] = l.begin()[i];
    }

    Vector(const Vector &other) : sz{other.sz}, elements{new T[other.sz]} {
        for (size_t i = 0; i < other.sz; i++) elements[i] = other.elements[i];
    }

    Vector(Vector &&other) : sz{other.sz}, elements{other.elements} {
        other.sz = 0;
        other.elements = nullptr;
    }

    ~Vector() { delete[] elements; }

    Vector &operator=(const Vector &other) {
        delete[] elements;
        sz = other.sz;
        elements = new T[sz];
        for (size_t i = 0; i < other.sz; i++) elements[i] = other.elements[i];
        return *this;
    }

    Vector &operator=(Vector &&other) {
        delete[] elements;
        sz = other.sz;
        elements = other.elements;
        other.sz = 0;
        other.elements = nullptr;
        return *this;
    }

    size_t size() { return sz; }

    T &operator[](size_t i) { return elements[i]; }

    std::string to_string() {
        std::stringstream ss;
        ss << "[";
        for (size_t i = 0; i < sz; i++) {
            ss << elements[i];
            if (i < sz - 1) ss << ", ";
        }
        ss << "]";
        return ss.str();
    }

    T *begin() { return elements; }

    T *end() { return elements + sz; }
};

int main() {
    Vector<char> a{'a', 'b', 'c'};
    std::cout << a.to_string() << std::endl;

    Vector<int> b{1, 2, 3};
    std::cout << b.to_string() << std::endl;

    Vector<std::string> c{"hello", "world"};
    for (auto &s : c) std::cout << s << std::endl;
}