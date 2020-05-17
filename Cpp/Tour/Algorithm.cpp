#include <algorithm>
#include <iostream>
#include <numeric>
#include <random>
#include <vector>

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::vector<T>& v) {
    out << "[";
    for (auto p = v.begin(); p < v.end(); p++) {
        out << *p;
        if (p < v.end() - 1) out << ", ";
    }
    out << "]";
    return out;
}

int main() {
    std::vector<int> v(10);

    std::iota(v.begin(), v.end(), 1);
    std::cout << v << std::endl;

    std::shuffle(v.begin(), v.end(), std::mt19937{std::random_device{}()});
    std::cout << v << std::endl;
}