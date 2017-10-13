//
//  Using member constructor
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class StudentId
{
public:
    StudentId(int idValue, int idValue2) : value(idValue), value2(idValue2)
    {
        cout << "Assign student id " << value << ' ' << value2 << endl;
    }
protected:
    int value, value2;
};

int nextStudentId = 1000;
class Student
{
public:
    Student () : name("No name")
    {
        cout << "Register student " << name << endl;
    }
    Student(const char* pName, int inId = nextStudentId++)
        : name(pName), id(inId, 2)
    {
        cout << "Register student " << name << endl;
    }
protected:
    string      name;
    StudentId   id{nextStudentId++, 4};
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s1("Barnabus", 1234);
    Student s2("Wizardcock");
    Student s3;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
