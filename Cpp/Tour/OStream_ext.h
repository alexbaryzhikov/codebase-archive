#include <forward_list>
#include <ostream>
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

template <typename T>
std::ostream& operator<<(std::ostream& out, const std::forward_list<T>& v) {
    out << "[";
    for (auto p = v.begin(); p != v.end();) {
        out << *p;
        if (++p != v.end())
            out << ", ";
        else
            break;
    }
    out << "]";
    return out;
}
