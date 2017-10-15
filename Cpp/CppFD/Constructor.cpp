//
//  Using class constructor
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Course
{
public:
    Course() { cout << "Constructing course..." << endl;}
};

class Student
{
public:
    Student()
    {
        cout << "Constructing student..." << endl;
        gpa = 0;
        semesterHours = 0;
    }
protected:
    int semesterHours;
    double gpa;
};

class Teacher
{
public:
    Teacher() { cout << "Constructing teacher..." << endl; }
protected:
    Course c;
};

class TutorPair
{
public:
    TutorPair()
    {
        cout << "Constructing TutorPair..." << endl;
        noMeetings = 0;
    }
protected:
    Teacher teacher;
    Student student;
    int noMeetings;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    cout << "Creating Student object." << endl;
    Student s;

    cout << "\nCreating Student object off the heap." << endl;
    Student *pS = new Student;

    cout << "\nCreating an array of 5 Student objects." << endl;
    Student studentRoster[5];

    cout << "\nCreating TutorPair." << endl;
    TutorPair tp;
}
