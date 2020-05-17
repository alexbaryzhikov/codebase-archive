#include <iostream>
#include <vector>

template <typename C, typename P>
int count(const C &vals, const P &pred) {
    int result = 0;
    for (const auto &e : vals)
        if (pred(e)) result++;
    return result;
}

template <typename T>
class less_than {
   public:
    T val;
    less_than(const T &val) : val{val} {}
    bool operator()(const T &x) const { return x < val; }
};

int main() {
    std::vector<int> a{0, 1, 2, 3, 4, 5};

    int val = 3;
    std::cout << count(a, less_than<int>{val}) << std::endl;
    std::cout << count(a, [&](int x) { return x < val; }) << std::endl;
}