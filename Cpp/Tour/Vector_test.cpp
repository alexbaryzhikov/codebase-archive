#include <iostream>
#include "Vector.h"

int main()
{
    Vector v(3);
    v[0] = 1;
    v[1] = 4;
    v[2] = 88;

    for (int i = 0; i < v.size(); i++) {
        std::cout << v[i] << ' ';
    }
}
