//
//  Protected class members
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
    friend void initialize(Student*);
public:
    double grade()
    {
        return gpa;
    }
    double grade(double newGpa)
    {
        double oldGpa = gpa;
        if (newGpa > 0 && newGpa <= 5.0)
        {
            gpa = newGpa;
        }
        return oldGpa;
    }
    int hours()
    {
        return semesterHours;
    }
    int hours(int newHours)
    {
        int oldHours = semesterHours;
        if (newHours >= 0)
        {
            semesterHours = newHours;
        }
        return oldHours;
    }
    void addCourse(int hours, double grade)
    {
        double weightedGpa = gpa * semesterHours;
        semesterHours += hours;
        weightedGpa += grade;
        gpa = weightedGpa / semesterHours;
    }
protected:
    double gpa{1};
    int semesterHours{1};
};

void initialize(Student *pS)
{
    pS->gpa = 0;
    pS->semesterHours = 0;
}

int main(int nNumberofArgs, char* pszArgs[])
{
    Student* s = new Student;
    initialize(s);
    int inputHours; double inputGrade;
    cout << "Initial semester hours:\n> ";
    cin >> inputHours;
    s->hours(inputHours);
    cout << "Initial grade:\n> ";
    cin >> inputGrade;
    s->grade(inputGrade);
    cout << endl << "The s has " << s->hours() <<
        " semester hours and average grade " << s->grade() << "." << endl;

}
