//
//  Using class destructors
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    Student() { cout << "Constructing Student object" << endl; }
    ~Student() { cout << "Destructing Student object" << endl; }
};

class Course
{
public:
    Course() { cout << "Constructing Course object" << endl; }
    ~Course() { cout << "Destructing Course object" << endl; }
};

class Teacher
{
public:
    Teacher()
    {
        cout << "Constructing Teacher object" << endl;
        pCourse = new Course;
    }
    ~Teacher()
    {
        cout << "Destructing Teacher object" << endl;
        delete pCourse;
    }
protected:
    Course* pCourse;
};

class TutorPair
{
public:
    TutorPair() { cout << "Constructing TutorPair object" << endl; }
    ~TutorPair() { cout << "Destructing TutorPair object" << endl; }
protected:
    Student student;
    Teacher teacher;
};

TutorPair* fn()
{
    cout << "Creating TutorPair object in fn()" << endl;
    TutorPair tP;
    cout << "Creating TutorPair object off the heap" << endl;
    TutorPair* pTP = new TutorPair;
    cout << "Returning from fn()" << endl;
    return pTP;
}

int main(int nNumberofArgs, char* pszArgs[])
{
TutorPair* pTPReturned = fn();
cout << "Returning pTPReturned object to the heap" << endl;
delete pTPReturned;

}
