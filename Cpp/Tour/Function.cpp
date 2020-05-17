#include <OStream_ext.h>

#include <algorithm>
#include <functional>
#include <iostream>
#include <vector>

std::function<int(double)> f;

int round(double x) { return static_cast<int>(x + 0.5); }

enum class round_style { truncate, round };

struct Round {
    round_style rs;

    Round(round_style rs) : rs{rs} {}

    int operator()(double x) const {
        return static_cast<int>(rs == round_style::truncate ? x : x + 0.5);
    }
};

int main() {
    f = round;
    std::cout << f(7.6) << std::endl;

    f = Round(round_style::truncate);
    std::cout << f(7.6) << std::endl;

    auto rs = round_style::round;
    f = [rs](double x) { return static_cast<int>(rs == round_style::round ? x + 0.5 : x); };
    std::cout << f(7.6) << std::endl;

    auto v = std::vector<double>{7.6, 4.0, 3.5, 6.9};
    f = Round(round_style::round);
    std::transform(v.begin(), v.end(), v.begin(), f);
    std::cout << v << std::endl;
}