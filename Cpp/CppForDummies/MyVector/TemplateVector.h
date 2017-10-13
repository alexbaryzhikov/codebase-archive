// TemplateVector.h
#pragma once
template <class T>
class TemplateVector
{
protected:
    T *array;
    int aSize, readPos, writePos;
public:
    TemplateVector(int sizeOfArray) : aSize(sizeOfArray)
    {
        array = new T[aSize];
        reset();
    }
    TemplateVector(const std::initializer_list<T> il) : TemplateVector(il.size())
    {
        for (const T *i=il.begin(); i<il.end(); ++i) add(*i);
    }
    int size() { return writePos; }
    void reset() { readPos = writePos = 0; }
    void add(const T &object) { if (writePos < aSize) array[writePos++] = object; }
    T& get() { return array[readPos++]; }
};
