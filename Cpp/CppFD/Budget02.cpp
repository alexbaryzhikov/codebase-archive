// Budget02.cpp : Defines the entry point for the console application.
//

#include <iostream>
using namespace std;

const int MAXACCOUNTS = 10;

class Checkings
{
protected:
	unsigned	number;
	double		balance;
public:
	Checkings(unsigned aN) :
		number(aN), balance(0.0) {}
	// access
	void displayInfo() { cout << " Account " << number << " = " << balance << '\n'; }
	double getBalance() { return balance; }
	// transactions
	void deposit(double x) { balance += x; }
	bool withdraw(double);
};

bool Checkings::withdraw(double x)
{
	if (x > balance)
	{
		cout << "ERROR: insufficient funds\n";
		cout << "balance " << balance << ", check " << x << '\n';
		return false;
	}
	else
	{
		// service fee
		if (balance < 500) balance -= 0.50;
		balance -= x;
	}
	return true;
}

class Savings
{
protected:
	unsigned	number;
	double		balance;
	int			noWithdrawals;
public:
	Savings(unsigned aN) :
		number(aN), balance(0.0), noWithdrawals(0) {}
	// access
	void displayInfo() { cout << " Account " << number << " = " << balance << '\n'; }
	double getBalance() { return balance; }
	// transactions
	void deposit(double x) { balance += x; }
	bool withdraw(double);
};

bool Savings::withdraw(double x)
{
	if (x > balance)
	{
		cout << "ERROR: insufficient funds\n";
		cout << "balance " << balance << ", withdrawal " << x << '\n';
		return false;
	}
	else
	{
		// >1 monthly withdrawal fee
		if (++noWithdrawals > 1) balance -= 5.0;
		balance -= x;
	}
	return true;
}

// forward declarations
void process(Checkings*);
void process(Savings*);

// account objects
Checkings*	accChe[MAXACCOUNTS];
Savings*	accSav[MAXACCOUNTS];

int main()
{
	char		userInput;
	int			noSav, noChe;
	unsigned	accN;
	double		total;
	cout << "This program creates Checkings and Savings accounts and accumulates balances\n";
	noSav = noChe = 0;
	// read accounts cycle
	for (;;)
	{
		cout << "Enter c for Chekings, s for Savings, x to exit:\n> ";
		cin >> userInput;
		if (userInput == 'x' || userInput == 'X') break;
		switch (userInput)
		{
		case 'C':
		case 'c':
			if (noChe >= MAXACCOUNTS) cout << "ERROR: no more room for Chekings account\n";
			else
			{
				cout << "Enter accout number:\n> ";
				cin >> accN;
				accChe[noChe] = new Checkings(accN);
				process(accChe[noChe]);
				++noChe;
			}
			break;
		case 'S':
		case 's':
			if (noSav >= MAXACCOUNTS) cout << "ERROR: no more room for Savings account\n";
			else
			{
				cout << "Enter accout number:\n> ";
				cin >> accN;
				accSav[noSav] = new Savings(accN);
				process(accSav[noSav]);
				++noSav;
			}
			break;
		default:
			cout << "ERROR: invalid command\n";
		}
	}
	// display total
	total = 0;
	cout << "\nCheckings accounts info:\n";
	for (int i = 0; i < noChe; ++i)
	{
		accChe[i]->displayInfo();
		total += accChe[i]->getBalance();
	}
	cout << "\nSavings accounts info:\n";
	for (int i = 0; i < noSav; ++i)
	{
		accSav[i]->displayInfo();
		total += accSav[i]->getBalance();
	}
	cout << "\nTotal = " << total << '\n';
	return 0;
}

void process(Checkings* aChe)
{
	double transaction;
	cout << "Enter positive number for deposite, negative number for check, zero to terminate\n";
	for (;;)
	{
		cout << "> ";
		cin >> transaction;
		if (transaction == 0) break;
		if (transaction > 0) aChe->deposit(transaction);
		else aChe->withdraw(-transaction);
	}
}

void process(Savings* aSav)
{
	double transaction;
	cout << "Enter positive number for deposite, negative number for withdrawal, zero to terminate\n";
	for (;;)
	{
		cout << "> ";
		cin >> transaction;
		if (transaction == 0) break;
		if (transaction > 0) aSav->deposit(transaction);
		else aSav->withdraw(-transaction);
	}
}
