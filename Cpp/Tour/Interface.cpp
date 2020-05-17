#include <iostream>
#include <memory>

class BaseClass {
   public:
    virtual ~BaseClass(){};
    virtual int get_value() const = 0;
};

class BaseClassImpl : public BaseClass {
    int *p;

   public:
    explicit BaseClassImpl(int v) : p{new int{v}} {
        std::cout << "construct " << v << std::endl;
    }

    ~BaseClassImpl() {
        std::cout << "destruct " << *p << std::endl;
        delete p;
    }

    int get_value() const override { return *p; }
};

int main() {
    auto a = std::make_unique<BaseClassImpl>(7);
    std::cout << "a: " << a->get_value() << std::endl;
}