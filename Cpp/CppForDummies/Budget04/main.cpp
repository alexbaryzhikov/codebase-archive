// main.cpp

#include <cstdlib>
#include <iostream>
#include "LinkedList.h"
using namespace std;

class Account;

template class LinkedList<Account>;
template class Node<Account>;

class Account
{
public:
	Account(unsigned _accNum) : number(_accNum),  balance(0.0)
	{
	    pNode = new Node<Account>(this);
	    ++count;
    }
	// selectors
	static int	        getNoAccounts() { return count; }
	virtual const char* getType() = 0;
	unsigned	        getNumber() { return number; }
	double		        getBalance() { return balance; }
	Node<Account>*      getNode() { return pNode; }
	void		        displayInfo() { cout << getType() << " account " << number << " = " << balance << '\n';  }
	// transactions
	void			    deposit(double _x) { balance += _x; }
	virtual bool	    withdraw(double);

protected:
	unsigned	    number;
	double		    balance;
	Node<Account>*	pNode;
	static int	    count;
};

int Account::count = 0;

bool Account::withdraw(double _x)
{
	if (_x > balance)
	{
		cout << "ERROR: insufficient funds.\n";
		return false;
	}
	else balance -= _x;
	return true;
}

class Checking : public Account
{
public:
	Checking(unsigned _accNum) : Account(_accNum) {}
	virtual const char* getType() { return "Checking"; }
	virtual bool withdraw(double);
};

bool Checking::withdraw(double _x)
{
	bool result = Account::withdraw(_x);
	if (result) { if (balance < 500) balance -= 0.50; }
	else { cout << "Account balance = " << balance << ", check = " << _x << '\n'; }
	return result;
}

class Savings : public Account
{
public:
	Savings(unsigned _accNum) : Account(_accNum), noWithdrawals(0) {}
	virtual const char* getType() { return "Savings"; }
	virtual bool withdraw(double);
protected:
	int noWithdrawals;
};

bool Savings::withdraw(double _x)
{
    bool result = Account::withdraw(_x);
	if (result) { if (++noWithdrawals > 1) balance -= 5.0; }
	else { cout << "account balance = " << balance << ", withdrawal = " << _x << '\n'; }
	return result;
}

void getAccounts(LinkedList<Account>*);
void displaySummary(LinkedList<Account>*);

int main()
{
	cout << "PROGRAM: Budget04\n";
	cout << "This program creates accounts, accumulates balances and stores them in a list\n";

	LinkedList<Account> accLL;

	getAccounts(&accLL);
	displaySummary(&accLL);

	system("PAUSE");
	return 0;
}

unsigned getNumber()
{
	unsigned n;
	cout << "Enter account number:\n> ";
	cin >> n;
	return n;
}

void process(Account* _pAcc)
{
	double d;
	cout << "Enter positive for deposit, negative for withdrawal, zero to terminate:\n";
	for (;;)
	{
		cout << "> ";
		cin >> d;
		if (d == 0.0) break;
		if (d > 0.0) _pAcc->deposit(d);
		else _pAcc->withdraw(-d);
	}
}

void getAccounts(LinkedList<Account>* _pAccLL)
{
	char c;
	Account* pNewAcc = nullptr;
	for (;;)
	{
		cout << "Enter c for Checking, s for Savings, x to terminate:\n> ";
		cin >> c;
		switch (c)
		{
		case 'c':
		case 'C':
			pNewAcc = new Checking(getNumber());
			break;
		case 's':
		case 'S':
			pNewAcc = new Savings(getNumber());
			break;
		case 'x':
		case 'X':
			break;
		default:
			cout << "ERROR: wrong request";
		}
		if (c == 'x' || c == 'X') break;
		if (pNewAcc != nullptr)
		{
		    _pAccLL->addNode(pNewAcc->getNode());
		    process(pNewAcc);
		}
	}
}

void displaySummary(LinkedList<Account>* _pAccLL)
{
	double total = 0.0;
	cout << "\nAccounts summary:\n";
	for (Node<Account>* pNode = _pAccLL->getFirstNode(); pNode; pNode = pNode->next())
	{
		Account* pAcc = pNode->getObject();
		pAcc->displayInfo();
		total += pAcc->getBalance();
	}
	cout << "\nTotal for all accounts = " << total << "\n\n";
}
