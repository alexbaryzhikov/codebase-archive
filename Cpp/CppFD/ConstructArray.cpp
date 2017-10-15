//
//  Template
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    Student(const char* _name, int _hours, double _gpa)
        : name(_name), semesterHours(_hours), gpa(_gpa)
    {
        cout << "Construct Student object, name = " << name << ", semesterHours = " << semesterHours << ", gpa = " << gpa << endl;
    }
    Student(const char* _name) : Student(_name, 0, 0.0)
    {
        cout << "passed... " << name << endl;
    }
    Student() : Student("No name", 0, 0.0)
    {
        cout << "passed... nothing" << endl;
    }
    ~Student()
    {
        cout << "Destruct Student " << name << endl;
    }
protected:
    const char* name;
    int         semesterHours;
    double      gpa;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s0;
    Student s1[] {"Benedict", "George"};
    Student s2[] { {"Bertram", 10, 5.5}, {"Graham", 14, 5.1} };

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

