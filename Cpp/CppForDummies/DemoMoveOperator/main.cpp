// The program demonstrates the move operator
//

#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <cstring>
using namespace std;

class MyContainer
{
protected:
	int size;
	char* pString;
	static void copyIt(MyContainer& tgt, const MyContainer& src)
	{
		cout << "Copying " << src.pString << endl;
		delete tgt.pString;
		tgt.size = src.size;
		tgt.pString = new char[tgt.size];
		strncpy(tgt.pString, src.pString, tgt.size);
	}
	static void moveIt(MyContainer& tgt, MyContainer& src)
	{
		cout << "Moving " << src.pString << endl;
		tgt.size = src.size;
		tgt.pString = src.pString;
		src.size = 0;
		src.pString = nullptr;
	}
public:
	MyContainer(int s, const char* str) : size(s)
	{
		cout << "Creating " << str << endl;
		pString = new char[size];
		strcpy(pString, str);
	}
	~MyContainer()
	{
		cout << "Destructing something" << endl;
		delete pString;
		pString = nullptr;
	}
	// copy constructor
	MyContainer(const MyContainer& src)
	{
		copyIt(*this, src);
	}
	MyContainer& operator=(const MyContainer& src)
	{
		delete pString;
		copyIt(*this, src);
		return *this;
	}
	// move constructor
	MyContainer(MyContainer&& src)
	{
		moveIt(*this, src);
	}
	MyContainer& operator=(MyContainer&& src)
	{
		delete pString;
		moveIt(*this, src);
		return *this;
	}
};

MyContainer fn(int s, const char* pStr)
{
	MyContainer a(s, pStr);
	return a;
}

int main()
{
	MyContainer mc(100, "Original");
	mc = fn(100, "Created in fn()");
	cout << "\nPress ENTER...";
	cin.get();
	return 0;
}
