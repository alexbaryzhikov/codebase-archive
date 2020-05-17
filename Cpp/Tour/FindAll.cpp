#include <OStream_ext.h>

#include <iostream>
#include <string>
#include <vector>

template <typename T>
using iter = typename T::iterator;

template <typename C, typename V>
std::vector<iter<C>> find_all(C& c, const V& v) {
    std::vector<iter<C>> result;
    for (auto p = c.begin(); p < c.end(); p++) {
        if (*p == v) result.push_back(p);
    }
    return result;
}

int main() {
    std::string s{"Mary had a little lamb"};
    for (auto p : find_all(s, 'a')) std::cout << *p;
    std::cout << std::endl;

    std::vector<int> v_int{1, 2, 3, 4, 5, 2};
    for (auto p : find_all(v_int, 2)) std::cout << *p;
    std::cout << std::endl;

    std::vector<std::string> v_str{"red", "green", "orange", "green"};
    for (auto p : find_all(v_str, "green")) *p = "blue";
    std::cout << v_str << std::endl;
}