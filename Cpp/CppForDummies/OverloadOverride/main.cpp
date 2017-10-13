//
//   Function Overload at compile time vs Override at runtime
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Student
{
public:
//    void calcTuition()
    virtual void calcTuition()
    {
        cout << "Student::calcTuition call" << endl;
    }
};

class GraduateStudent : public Student
{
public:
    void calcTuition()
    {
        cout << "GraduateStudent::calcTuition call" << endl;
    }
};

void fn(Student& s)
{
    s.calcTuition();
}

int main(int nNumberofArgs, char* pszArgs[])
{
    Student s1;
    fn(s1);
    GraduateStudent s2;
    fn(s2);

    cout << "\nPress ENTER to return from main()";
    cin.get();
    return 0;
}
