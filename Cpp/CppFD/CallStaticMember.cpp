//
//  Calling static function different ways
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Person
{
protected:
    static int  numberOfPeople;
    string      personName;

public:
    Person(const char* pName = "no name")
        : personName(pName)
    {
        numberOfPeople++;
    }
    ~Person()
    {
        numberOfPeople--;
    }
    const string& name()
    {
        return personName;
    }
    static int number()
    {
        return numberOfPeople;
    }
};

int Person::numberOfPeople = 0;

int main(int nNumberofArgs, char* pszArgs[])
{
    // create two persons
    Person p1("Dwight");
    Person* p2 = new Person("Grisha");
    cout << "Created " << p1.name() << " and " << p2->name() << endl;
    // case 1: ask object
    cout << "Number of people: " << p1.number() << endl;
    // destruct one person
    cout << "Deleting " << p2->name() << endl;
    delete p2;
    // case 2: ask class
    cout << "Number of people: " << Person::number() << endl;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

