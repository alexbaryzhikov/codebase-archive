#include "AccountLinkedList.h"
using namespace Lists;

void AccountLinkedList::addNode(Node* node)
{
	node->nextNode = headNode;
	headNode = node;
}
