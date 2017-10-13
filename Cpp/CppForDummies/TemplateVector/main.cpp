//
//  Template Vector
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include "TemplateVector.h"
using namespace std;

void intFn()
{
    TemplateVector<int> integers(10);
    cout << "Enter integer numbers to store, negative to terminate\n";
    for (;;)
    {
        int num;
        cout << "> ";
        cin >> num;
        if (num<0) break;
        integers.add(num);
    }
    cout << "\nNumbers you've entered:\n";
    cout << integers.get();
    for (int i=1; i<integers.size(); ++i)
    {
        cout << ", " << integers.get();
    }
    cout << "\n\n";
}

class Name
{
protected:
    string name;
public:
    Name() = default;
    Name(string s) : name(s) {}
    const string& display() { return name; }
};

void nameFn()
{
    TemplateVector<Name> names(20);
    cout << "Enter names to store, x to terminate\n";
    for (;;)
    {
        string n;
        cout << "> ";
        cin >> n;
        if (n=="x"||n=="X") break;
        names.add(Name(n));
    }
    cout << "\nNames you've entered:\n";
    Name& nN = names.get();
    cout << nN.display();
    for (int i=1; i<names.size(); ++i)
    {
        nN = names.get();
        cout << ", " << nN.display();
    }
    cout << "\n\n";
}

int main()
{
    intFn();
    nameFn();
    system("PAUSE");
    return 0;
}

