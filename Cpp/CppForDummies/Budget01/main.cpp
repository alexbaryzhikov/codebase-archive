// Budget01.cpp : Defines the entry point for the console application.
//

#include <iostream>
using namespace std;

const int maxNoAccounts = 10;

// accounts array
unsigned	accNumber[maxNoAccounts] = {};
double		accBalance[maxNoAccounts] = {};

void init(unsigned*);
void process(double*);

int main()
{
	int			noAccounts = 0;
	char		userInput = 0;
	double		total = 0;

	cout << "This program creates accounts and accumulates their balance\n";
	cout << "Enter x to exit or c to continue:\n> ";
	cin >> userInput;
	if (userInput == 'x' || userInput == 'X') return 0;

	// read accouts data cycle
	while (noAccounts < maxNoAccounts)
	{
		cout << "Enter x to exit or c to create account:\n> ";
		cin >> userInput;
		if (userInput == 'x' || userInput == 'X') break;
		init(&accNumber[noAccounts]);
		process(&accBalance[noAccounts]);
		++noAccounts;
	}

	// display total
	for (int i = 0; i < noAccounts; ++i)
	{
		cout << "Accout " << accNumber[i] << " = " << accBalance[i] << '\n';
		total += accBalance[i];
	}
	cout << "Total for all accounts = " << total << '\n';

    return 0;
}

void init(unsigned* nA)
{
	cout << "Enter account number:\n> ";
	cin >> *nA;
}

void process(double* nB)
{
	double userTransaction;
	cout << "Enter positive number to deposit, negative to withdraw, 0 to terminate:\n";
	for (;;)
	{
		cout << "> ";
		cin >> userTransaction;
		if (userTransaction == 0) return;
		if (userTransaction > 0) *nB += userTransaction;
		else
		{
			userTransaction = -userTransaction;
			if (userTransaction > *nB)
			{
				cout << "ERROR: Insufficient funds\n"
					<< "balance = " << *nB << ", check = " << userTransaction << '\n';
			}
			else *nB -= userTransaction;
		}
	}
}
