//
//  Constructing a copy of an object
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    // conventional constructor
    Student(const char* pName = "no name", int ssID = 0)
        : name(pName), id(ssID)
    {
        cout << "Constructing " << name << endl;
    }
    // copy constructor
    Student(const Student& s)
        : name("Copy of " + s.name), id(s.id)
    {
        cout << "Constructing " << name << endl;
    }
    ~Student()
    {
        cout << "Destructing " << name << endl;
    }
protected:
    string  name;
    int     id;
};


// function gets object as value
void fn(Student s)
{
    cout << "Running fn()" << endl;
}


int main(int nNumberofArgs, char* pszArgs[])
{
    Student borg("Borg", 1234);
    cout << "Calling fn()" << endl;
    fn(borg);
    cout << "Back to main()" << endl;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

