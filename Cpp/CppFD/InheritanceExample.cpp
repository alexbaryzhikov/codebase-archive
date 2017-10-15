//
//  Demonstrates an inheritance relationship in which the subclass constructor
//      sends the argument information to the constructor in the base class
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Advisor {};  // define an empty class

class Student
{
protected:
    string      sName;
    int         semesterHours;
    double      semesterGPA;
public:
    Student(const char* inName = "no name")
        : sName(inName), semesterHours(0), semesterGPA(0.0)
    {
        cout << "Constructing student " << sName << endl;
    }
    ~Student()
    {
        cout << "Destructing student " << sName << endl;
    }
    void addCourse(int inHours, double inGPA)
    {
        cout << "Adding course to student " << sName << endl;
        semesterGPA = semesterGPA*semesterHours + inGPA*inHours;
        semesterHours += inHours;
        semesterGPA /= semesterHours;
    }
    string name()
    {
        return sName;
    }
    int hours()
    {
        return semesterHours;
    }
    double gpa()
    {
        return semesterGPA;
    }
};

class GraduateStudent : public Student
{
protected:
    Advisor&    adv;
    double      qualifierGrade;
public:
    GraduateStudent(const char* inName, Advisor& inAdv, double inGrade = 0.0)
        : Student(inName), adv(inAdv), qualifierGrade(inGrade)
    {
        cout << "Counstructing graduate student " << sName << endl;
    }
    ~GraduateStudent()
    {
        cout << "Destructing graduate student " << sName << endl;
    }
    double qualifier()
    {
        return qualifierGrade;
    }
};

int main(int nNumberofArgs, char* pszArgs[])
{
    // create a dummy advisor to give to GraduateStudent
    Advisor adv;

    // create two student types
    Student s1("Maryana Jennifer");
    GraduateStudent s2(
        "Benjamin Cox",     // student name
        adv,                // advisor
        10.0);              // qualifier grade

    // add courses to students
    s1.addCourse(
        10,                 // course hours
        4.0);               // grade
    s2.addCourse(
        15,                 // course hours
        6.0);               // grade

    // displayer the graduate student's qualifier grade
    cout << "Qualifier grade of " << s2.name() << " is " << s2.qualifier() << endl;

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}

