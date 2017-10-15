// Program is about overloading assignment operator
//

#include <iostream>
using namespace std;

class DArray   // dynamic array class
{
protected:
	void copyDArray(const DArray&);
	void deleteDArray();
	int		arrayLength;
	int*	pArray;

public:
	DArray(int inLength) :
		arrayLength(inLength), pArray(nullptr)
	{
		cout << "Creating DArray of length = " << arrayLength << '\n';
		if (arrayLength > 0) pArray = new int[arrayLength];
	}

	~DArray()
	{
		deleteDArray();
	}

	DArray(const DArray& inDA)
	{
		cout << "Copying DArray of length = " << inDA.arrayLength << '\n';
		copyDArray(inDA);
	}

	DArray& operator=(const DArray& inDA)
	{
		cout << "Assigning source of length = " << inDA.arrayLength << "\n";
		cout << "to the target of length = " << this->arrayLength << "\n";
		deleteDArray();
		copyDArray(inDA);
		return *this;
	}

	int& operator[](int i)
	{
		return pArray[i];
	}

	int size() { return arrayLength; }

	void display(ostream& out)
	{
		if (arrayLength > 0)
		{
			out << pArray[0];
			for (int i = 1; i < arrayLength; ++i)
				out << ", " << pArray[i];
		}
	}
};

void DArray::copyDArray(const DArray& inDA)
{
	arrayLength = inDA.arrayLength;
	pArray = nullptr;
	if (arrayLength > 0)
	{
		pArray = new int[arrayLength];
		for (int i = 0; i < arrayLength; ++i)
			pArray[i] = inDA.pArray[i];
	}
}

void DArray::deleteDArray()
{
	arrayLength = 0;
	delete pArray;
	pArray = nullptr;
}

int main()
{
	DArray da1(5);
	for (int i = 0; i < da1.size(); ++i)
		da1[i] = i;
	cout << "da1 = "; da1.display(cout); cout << "\n";
	DArray da2 = da1;
	da2[2] = 32;
	cout << "da2 = "; da2.display(cout); cout << "\n";
	da2 = da1;
	cout << "da2 = "; da2.display(cout); cout << "\n";
    return 0;
}

