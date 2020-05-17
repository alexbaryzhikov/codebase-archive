#include <OStream_ext.h>

#include <algorithm>
#include <iostream>
#include <iterator>
#include <string>

template <typename C>
using val_type = typename C::value_type;

template <typename C>
using iter_type = typename C::iterator;

template <typename I>
using iter_category = typename std::iterator_traits<I>::iterator_category;

template <typename I>
void sort_helper(I begin, I end, std::random_access_iterator_tag) {
    std::sort(begin, end);
}

template <typename I>
void sort_helper(I begin, I end, std::forward_iterator_tag) {
    std::vector<val_type<I>> v(begin, end);
    std::sort(v.begin(), v.end());
    std::move(v.begin(), v.end(), begin);
}

template <typename C>
void sort(C& c) {
    sort_helper(c.begin(), c.end(), iter_category<iter_type<C>>{});
}

int main() {
    auto v = std::vector<std::string>{"bar", "foo", "baz"};
    auto l = std::forward_list<std::string>{"moo", "sap", "crop"};

    sort(v);
    sort(l);

    std::cout << v << std::endl;
    std::cout << l << std::endl;
}
