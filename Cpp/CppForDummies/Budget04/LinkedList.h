// LinkedList.h

#ifndef LINKEDLIST_H_INCLUDED
#define LINKEDLIST_H_INCLUDED

template <class T> class LinkedList;

template <class T> class Node
{
public:
                Node(T* _pObject) { pObject = _pObject; pNext = nullptr; }
    T*          getObject() { return pObject; }
    Node<T>*    next() { return pNext; }
    Node<T>*    next(Node<T>* _pNode) { pNext = _pNode; return pNext; }

protected:
    Node<T>*    pNext;
    T*          pObject;
};

template <class T> class LinkedList
{
public:
                LinkedList() : pFirst(nullptr) {}
    Node<T>*    getFirstNode() { return pFirst; }
    Node<T>*    getLastNode()
    {
        if (!pFirst) return nullptr;
        Node<T>* pLast = pFirst;
        while (pLast->next())
        {
            pLast = pLast->next();
        }
        return pLast;
    }
    void        addNode(Node<T>* _pNode)
    {
         Node<T>* pNode = getLastNode();
         if (!pNode) pFirst = _pNode;
         else pNode->next(_pNode);
    }

protected:
    Node<T>*    pFirst;
};

#endif // LINKEDLIST_H_INCLUDED
