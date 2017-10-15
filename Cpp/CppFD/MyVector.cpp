//
//  My Vector
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include "TemplateVector.h"
using namespace std;

int main()
{
    TemplateVector<int> MyVector{10, 20, 30, 40, 50};
    cout << MyVector.get();
    for (int i=1; i<MyVector.size(); ++i) cout << ", " << MyVector.get();
    cout << "\n\n";
    system("PAUSE");
    return 0;
}

