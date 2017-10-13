//
//  Constructing a copy of an object
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Person
{
public:
    Person(const char* pN)
    {
        cout << "Constructing " << pN << endl;
        pName = new string(pN);
    }
    ~Person()
    {
        cout << "Destructing " << pName << " (" << *pName << ")" << endl;
        *pName = "Oops... somebody deleted this asset";
        // delete pName;
    }
    string*      pName;
};

void fn()
{
    // create a new object
    Person p1("Borg");
    // copy the contents of p1 into p2
    Person p2(p1);
}

int main(int nNumberofArgs, char* pszArgs[])
{
    cout << "Calling fn()" << endl;
    fn();
    cout << "Back to main()" << endl;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

