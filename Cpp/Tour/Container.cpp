#include <iostream>
#include <list>
#include <vector>

class Container {
   public:
    virtual double &operator[](size_t) = 0;
    virtual size_t size() const = 0;
    virtual ~Container() {}
};

class VectorContainer : public Container {
    std::vector<double> v;

   public:
    VectorContainer(size_t n) : v(n) {}

    VectorContainer(std::initializer_list<double> l) : v(l) {}

    ~VectorContainer() {}

    double &operator[](size_t i) { return v[i]; }

    size_t size() const { return v.size(); }
};

class ListContainer : public Container {
    std::list<double> elements;

   public:
    ListContainer() {}

    ListContainer(std::initializer_list<double> l) : elements{l} {}

    ~ListContainer() {}

    double &operator[](size_t i) {
        for (auto &x : elements) {
            if (i == 0) return x;
            --i;
        }
        throw std::out_of_range("List container");
    }

    size_t size() const { return elements.size(); }
};

void use(Container &c) {
    const size_t sz = c.size();
    for (size_t i = 0; i < sz; i++) {
        std::cout << c[i] << ' ';
    }
    std::cout << '\n';
}

int main() {
    VectorContainer vc{10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    use(vc);

    ListContainer lc{1, 2, 3, 4, 5, 6, 7, 8, 9};
    use(lc);
}
