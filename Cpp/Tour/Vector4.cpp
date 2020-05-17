#include <iostream>
#include <vector>

template <typename T>
class Vector {
    T *head;
    T *next;
    T *last;

   public:
    explicit Vector(size_t n) : head{new T[n]}, next{head}, last{head + n} {}

    Vector(std::initializer_list<T> l)
        : head{new T[l.size()]}, next{head + l.size()}, last{head + l.size()} {
        for (size_t i = 0; i < l.size(); i++) head[i] = l.begin()[i];
    }

    ~Vector() { delete[] head; }

    size_t size() const { return next - head; }
    size_t capacity() const { return last - head; }
    T &operator[](size_t i) { return head[i]; }
    const T &operator[](size_t i) const { return head[i]; }

    void reserve(size_t);
    void push_back(const T &);
    void push_back(T &&);
};

template <typename T>
void Vector<T>::reserve(size_t n) {
    size_t k = n < size() ? n : size();
    T *new_head{new T[n]};
    for (size_t i = 0; i < k; i++) new_head[i] = std::move(head[i]);

    delete[] head;
    head = new_head;
    next = head + k;
    last = head + n;
}

template <typename T>
void Vector<T>::push_back(const T &t) {
    std::cout << "push_back copy" << std::endl;
    if (next == last) reserve(size() == 0 ? 8 : size() * 2);
    *next = t;
    next++;
}

template <typename T>
void Vector<T>::push_back(T &&t) {
    std::cout << "push_back move" << std::endl;
    if (next == last) reserve(size() == 0 ? 8 : size() * 2);
    *next = std::move(t);
    next++;
}

template <typename T>
std::ostream &operator<<(std::ostream &out, const Vector<T> &v) {
    out << "[";
    for (size_t i = 0; i < v.size(); i++) {
        out << v[i];
        if (i < v.size() - 1) out << ", ";
    }
    out << "]"
        << " (" << v.size() << ", " << v.capacity() << ")";
    return out;
}

struct Entry {
    int id;

    Entry() : id{0} {}

    explicit Entry(int id) : id{id} {}

    Entry(const Entry &e) : id{e.id} { std::cout << "constr copy entry" << std::endl; }

    Entry(Entry &&e) : id{e.id} {
        std::cout << "coustr move entry" << std::endl;
        e.id = 0;
    }

    Entry &operator=(const Entry &e) {
        std::cout << "assign copy entry" << std::endl;
        id = e.id;
        return *this;
    }

    Entry &operator=(Entry &&e) {
        std::cout << "assign move entry" << std::endl;
        id = e.id;
        e.id = 0;
        return *this;
    }
};

std::ostream &operator<<(std::ostream &out, const Entry &e) {
    return out << "Entry(" << e.id << ")";
}

Entry f() { return Entry{7}; }

int main() {
    Vector<Entry> v2(2);
    Entry e1{1};
    Entry e2{2};
    v2.push_back(e1);
    v2.push_back(std::move(e2));
    std::cout << e1 << std::endl << e2 << std::endl << v2 << std::endl;

    Entry e3 = f();
    std::cout << e3 << std::endl;

    v2.push_back(e3);  // reallocation happens here
    std::cout << v2 << std::endl;

    v2.push_back(f());
    std::cout << v2 << std::endl;
}