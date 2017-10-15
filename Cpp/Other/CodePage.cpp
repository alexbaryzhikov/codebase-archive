//
//  Testing playground
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <windows.h>
using namespace std;

int main()
{
    UINT WINAPI defaultCP = GetConsoleOutputCP();
    cout << defaultCP << '\n';
    cout << SetConsoleOutputCP(1251) << '\n';
    for(int i = 0; i <= 255; i++)
    {
        cout << (char)i << '\t';
    }
    cout << '\n' << "--- cut ---\n";
    cout << SetConsoleOutputCP(defaultCP) << '\n';
    for(int i = 0; i <= 255; i++)
    {
        cout << (char)i << '\t';
    }
	return 0;
}
