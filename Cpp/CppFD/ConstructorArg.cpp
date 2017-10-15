//
//  Using object constructor with arguments
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    Student(const char* pName, int xHours, double xGPA)
    {
        cout << "Constructing student " << pName << endl;
        name = pName;
        semesterHours = xHours;
        gpa = xGPA;
    }
    Student() : Student("Noname", 0, 0.0) {}
    Student(const char* pName) : Student(pName, 0, 0.0) {}
    ~Student()
    {
        cout << "Demolishing student " << name << endl;
    }

protected:
    string  name;
    int     semesterHours;
    double  gpa;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s1;
    Student* pS2 = new Student("Hesus");
    Student s3("Jake Sully", 10, 2.0);
    delete pS2;
    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
