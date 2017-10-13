#pragma once

class Account;
namespace Lists
{
	class Node;

	class AccountLinkedList
	{
	protected:
		Node* headNode;
	public:
		AccountLinkedList() { headNode = 0; }
		Node* getFirstNode() { return headNode; }
		void addNode(Node*);
	};

	class Node
	{
		friend class AccountLinkedList;
	protected:
		Account*			acnt;
		AccountLinkedList*	nodeList;
		Node*				nextNode;
	public:
		Node(AccountLinkedList* list, Account* acc) :
			acnt(acc), nodeList(list), nextNode(0)
		{
			list->addNode(this);
		}
		static Node*		getFirstNode(AccountLinkedList* list) { return list->getFirstNode(); }
		Node*				getNextNode() { return nextNode; }
		Account*			getAccount() { return acnt; }
		AccountLinkedList*	getList() { return nodeList; }
	};
}
