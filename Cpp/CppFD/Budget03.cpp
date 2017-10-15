// Budget03.cpp : Defines the entry point for the console application.
//

#include <iostream>
#include "AccountLinkedList.h"
using namespace std;
using namespace Lists;

class Account
{
protected:
	unsigned	number;
	double		balance;
	Node		node;
	static int	count;
public:
	Account(unsigned aN, AccountLinkedList* aL) :
		node(aL, this), number(aN),  balance(0.0)
	{
		++count;
	}
	// access
	unsigned	getNumber() { return number; }
	double		getBalance() { return balance; }
	static int	getNoAccounts() { return count; }
	void		displayInfo() { cout << getType() << " account " << number << " = " << balance << '\n';  }
	// transactions
	void			deposit(double x) { balance += x; }
	virtual bool	withdraw(double);
	// account type
	virtual const char* getType() = 0;
};

int Account::count = 0;

bool Account::withdraw(double x)
{
	if (x > balance)
	{
		cout << "ERROR: insufficient funds\n";
		return false;
	}
	else balance -= x;
	return true;
}

class Checkings : public Account
{
public:
	Checkings(unsigned aN, AccountLinkedList* aL) :
		Account(aN, aL) {}
	virtual bool withdraw(double);
	virtual const char* getType() { return "Checking"; }
};

bool Checkings::withdraw(double x)
{
	if (Account::withdraw(x))
	{
		if (balance < 500) balance -= 0.50;
	}
	else
	{
		cout << "account balance = " << balance << ", check = " << x << '\n';
		return false;
	}
	return true;
}

class Savings : public Account
{
protected:
	int noWithdrawals;
public:
	Savings(unsigned aN, AccountLinkedList* aL) :
		Account(aN, aL), noWithdrawals(0) {}
	virtual bool withdraw(double);
	virtual const char* getType() { return "Saving"; }
};

bool Savings::withdraw(double x)
{
	if (Account::withdraw(x))
	{
		if (++noWithdrawals > 1) balance -= 5.0;
	}
	else
	{
		cout << "account balance = " << balance << ", withdrawal = " << x << '\n';
		return false;
	}
	return true;
}

// forward declarations
void getAccounts(AccountLinkedList*);
void displaySummary(AccountLinkedList*);

int main()
{
	cout << "Budget03\n";
	cout << "This program creates accounts, accumulates balances and stores them in a list\n";
	AccountLinkedList* aList = new AccountLinkedList();
	getAccounts(aList);
	displaySummary(aList);
	return 0;
}

unsigned getNumber()
{
	unsigned a;
	cout << "Enter account number:\n> ";
	cin >> a;
	return a;
}

void process(Account* nAcc)
{
	double a;
	cout << "Enter positive for deposit, negative for withdrawal, zero to terminate:\n";
	for (;;)
	{
		cout << "> ";
		cin >> a;
		if (a == 0.0) break;
		if (a > 0.0) nAcc->deposit(a);
		else nAcc->withdraw(-a);
	}
}

void getAccounts(AccountLinkedList* acntList)
{
	char c;
	Account* newAcc = nullptr;
	for (;;)
	{
		cout << "Enter c for Checking, s for Saving, x to terminate:\n> ";
		cin >> c;
		switch (c)
		{
		case 'c':
		case 'C':
			newAcc = new Checkings(getNumber(), acntList);
			break;
		case 's':
		case 'S':
			newAcc = new Savings(getNumber(), acntList);
			break;
		case 'x':
		case 'X':
			break;
		default:
			cout << "ERROR: wrong request";
		}
		if (c == 'x' || c == 'X') break;
		if (newAcc != nullptr) process(newAcc);
	}
}

void displaySummary(AccountLinkedList* acntList)
{
	double total = 0.0;
	cout << "\nAccounts summary:\n";
	for (Node* node = acntList->getFirstNode(); node != 0; node = node->getNextNode())
	{
		Account* acnt = node->getAccount();
		acnt->displayInfo();
		total += acnt->getBalance();
	}
	cout << "\nTotal for all accounts = " << total << '\n';
}
