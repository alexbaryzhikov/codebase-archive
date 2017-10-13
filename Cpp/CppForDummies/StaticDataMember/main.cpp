//
//  Using static data member
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Person
{
public:
    static int  numberOfPeople;
    string      name;

    Person(const char* pName = "no name")
        : name(pName)
    {
        numberOfPeople++;
    }
    ~Person()
    {
        numberOfPeople--;
    }
};

int Person::numberOfPeople = 0;

int main(int nNumberofArgs, char* pszArgs[])
{
    Person p1, p2("Grisha");
    cout << "Number of sorry ass bastards: " << Person::numberOfPeople << endl;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

