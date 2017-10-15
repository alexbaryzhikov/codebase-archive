//
//  Call member function demo
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
    double addCourse(int hours, double grade)
    {
        double weightedGPA = gpa * semesterHours;
        semesterHours += hours;
        weightedGPA += grade * hours;
        if (semesterHours == 0)
        {
            return 0;
        }
        gpa = weightedGPA / semesterHours;
        return gpa;
    }
    int semesterHours;
    double gpa;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s;
    s.semesterHours = 0;
    s.gpa = 0;
    for (;;)
    {
        int addHours;
        double addGrade;
        cout << "Add course HOURS: ";
        cin >> addHours;
        cout << "Add course GRADE: " ;
        cin >> addGrade;
        s.addCourse(addHours, addGrade);
        cout << endl << "New total hours: " << s.semesterHours << endl;
        cout << "New average grade: " << s.gpa << endl;
        char cCont;
        cout << "Continue? ";
        cin >> cCont;
        cout << endl;
        if (cCont == 'n')
        {
            break;
        }
    }
}
