//
// Custom Exception Class
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>

using namespace std;

class MyException
{
protected:
    string mesg, funcName, fileName;
    int errorValue, lineNum;
public:
    MyException(const char *msg, int n, string func, string file, int line) : mesg(msg), errorValue(n), funcName(func), fileName(file), lineNum(line) {}
    virtual string display()
    {
        ostringstream out;
        out << "ERROR: " << mesg << ", error value is " << errorValue << "\nIn function " << funcName << "(), file " << fileName << ", line " << lineNum << "\n";
        return out.str();
    }
};

long fact(int n)
{
    if (n<0)
    {
        throw MyException("The argument for factorial is negative", n, __func__, __FILE__, __LINE__);
    }
    long result = 1;
    while (n>0) result *= n--;
    return result;
}

int main()
{
    try
    {
        cout << "The factorial of 3 is " << fact(3) << "\n";
        cout << "The factorial of -4 is " << fact(-4) << "\n";
        cout << "The factorial of 5 is " << fact(5) << "\n";
    }
    catch (MyException error)
    {
        cout << error.display();
    }
    catch (...)
    {
        cout << "ERROR: No can do.\n";
    }
    system("PAUSE");
    return 0;
}
