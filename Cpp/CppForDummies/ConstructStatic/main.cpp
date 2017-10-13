//
//  Template
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class DoNothing
{
public:
    DoNothing(int val) : value(val)
    {
        cout << "Construct DoNothing with value " << value << endl;
    }
    ~DoNothing()
    {
        cout << "Destroy DoNothing with value " << value << endl;
    }
    int value;
};

void fn(int val)
{
    cout << "Call fn() with value " << val << endl;
    static DoNothing dn(val);
    cout << dn.value << endl;
}

int main(int nNumberofArgs, char* pszArgs[])
{
    fn(10);
    fn(20);

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
