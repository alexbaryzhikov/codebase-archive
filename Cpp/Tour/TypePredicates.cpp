#include <iostream>
#include <string>
#include <type_traits>

template <typename T>
constexpr bool is_arithmetic = std::is_arithmetic<T>::value;

template <typename Scalar>
struct complex {
    Scalar re;
    Scalar im;

    static_assert(is_arithmetic<Scalar>, "Requires arithmetic type.");

    complex(Scalar re, Scalar im) : re{re}, im{im} {}
};

template <typename T>
std::ostream& operator<<(std::ostream& out, complex<T>& c) {
    out << '{' << c.re << ", " << c.im << '}';
    return out;
}

int main() {
    std::cout << is_arithmetic<int> << std::endl;
    std::cout << is_arithmetic<std::string> << std::endl;
    std::cout << is_arithmetic<bool> << std::endl;

    complex<int> a{1, 2};
    std::cout << a << std::endl;

    complex<std::string> b{"foo", "bar"};
}