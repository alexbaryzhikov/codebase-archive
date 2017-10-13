//
//  Using default object constructor
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    Student () = default;
    Student(const char* pName)
    {
        name = pName;
    }
    ~Student()
    {
        cout << "Demolishing student " << name << endl;
    }
    string name{"Stephen"};
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s1;
    cout << s1.name << endl;
    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
