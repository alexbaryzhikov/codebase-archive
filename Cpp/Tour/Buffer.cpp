#include <iostream>

template <typename T, size_t N>
struct Buffer {
    using value_type = T;
    constexpr size_t size() { return N; }
    T vals[N];
};

int main() {
    Buffer<int, 10> b;
    std::cout << b.size() << std::endl;
}