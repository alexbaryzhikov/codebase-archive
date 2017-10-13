//
//  Sine calculation
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <math.h>
#include <windows.h>
#define PI		3.14159265358979323846

using namespace std;

int fact(int n)
{
  return (n == 1 || n == 0) ? 1 : fact(n - 1) * n;
}

int main()
{
	// cos(x)= 1 + x - x^2/2! +x^4/4! - x^6/6! + ...
	// sin(x)= x - x^3/3! + x^5/5! - x^7/7! + ...
    float x;
    for(;;)
    {
        cout << "> enter x (0..45 deg)\n> ";
        scanf("%f", &x);
        x = x*PI/180;
        float sine = x-pow(x, 3)/fact(3)+pow(x, 5)/fact(5)-pow(x, 7)/fact(7)+pow(x, 9)/fact(9);
        cout << "       sine x = " << sine << endl;
        cout << "math.h sine x = " << sin(x) << endl;
    }
	return 0;
}
