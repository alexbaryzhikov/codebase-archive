//
//  STL List Students
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <list>
using namespace std;

class Student
{
public:
    int id;
    string name;
    Student(const char *aName, int aID) : name(aName), id(aID) {}
};

bool operator<(const Student &s1, const Student &s2)
{
    return s1.id<s2.id;
}

void displayStudentList(list<Student> &studList)
{
    for(list<Student>::iterator i=studList.begin(); i!=studList.end(); ++i)
    {
        cout.width(4);
        cout << i->id << " : " << i->name << endl;
    }
}

int main()
{
    list<Student> students;
    students.push_back(Student("Valcor Manson", 100));
    students.push_back(Student("Biggie Satan", 666));
    students.push_back(Student("Lester Pusher", 4));
    cout << "Initial student list:\n";
    displayStudentList(students);
    students.sort();
    cout << "\nSorted student list:\n";
    displayStudentList(students);
    cout << endl;
    system("PAUSE");
    return 0;
}
