//
//  Moving temporary instead of creating a copy
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Person
{
protected:
    string*      pName;

public:
    Person(const char* pN)
    {
        cout << "Constructing " << pN << endl;
        pName = new string(pN);
    }
    Person(Person& person)
    {
        cout << "Copying " << *person.pName << endl;
        pName = new string("Copy of "+*person.pName);
        //*pName += *person.pName;
    }
    Person(Person&& person)
    {
        cout << "Moving " << *person.pName << endl;
        pName = person.pName;
        person.pName = nullptr;
    }
    ~Person()
    {
        if(pName)
        {
            cout << "Destructing " << *pName << endl;
            delete pName;
        }
        else
        {
            cout << "Destructing null object" << endl;
        }
    }
};

Person fn2(Person pFn2)
{
    cout << "Entering fn2()" << endl;
    return pFn2;
}

Person fn1(char* pName)
{
    cout << "Entering fn1()" << endl;
    Person* pFn1 = new Person(pName);
    return fn2(*pFn1);
}

int main(int nNumberofArgs, char* pszArgs[])
{
    cout << "Calling fn1()" << endl;
    Person pMain(fn1("Boreas"));
    cout << "Back to main()" << endl;

    system("PAUSE");
    return 0;
}

