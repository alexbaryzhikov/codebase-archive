//
//  Passing object to outside function by object pointer
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    int semesterHours;
    double gpa;
};

void someFn(Student* pS)
{
    pS->gpa = 10;
    cout << "pS->gpa = " << pS->gpa << endl;
}

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s;
    s.semesterHours = 0;
    s.gpa = 0;
    cout << "s.gpa = " << s.gpa << endl;
    cout << "Calling someFn...\n";
    someFn(&s);
    cout << "Returned from someFn\n";
    cout << "s.gpa = " << s.gpa << endl;

}
