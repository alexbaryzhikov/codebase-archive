#include "Vector.h"

Vector::Vector(int n) : elements{new double[n]}, n{n} {
    for (int i = 0; i < n; i++) {
        elements[i] = 0;
    }
}

Vector::~Vector() { delete[] elements; }

double &Vector::operator[](int i) { return elements[i]; }

int Vector::size() { return n; }
