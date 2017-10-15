//
//  Make a picture
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <conio.h>
#include <math.h>
#include "DrawLine.h"

using namespace std;

struct LineSegment
{
    Vect segStart, segEnd ;
    LineSegment(Vect a, Vect b) : segStart(a), segEnd(b) {}
    LineSegment() : LineSegment(Vect(0,0), Vect(0,0)) {}
};

struct SegmentsArray
{
    int arraySize;
    LineSegment* pSegments;
    SegmentsArray(int noSegments) : arraySize(noSegments), pSegments(nullptr) { if (arraySize > 0) pSegments = new LineSegment[arraySize]; }
    ~SegmentsArray() { delete pSegments; }
    LineSegment& operator[](int i) { return pSegments[i]; }
};

struct Rect
{
    Vect origin;
    int horiz, vert;
    Rect(Vect originVect, int horizSize, int vertSize) : origin(originVect), horiz(horizSize), vert(vertSize) {}
};

Vect sumVect(Vect a, Vect b) { return Vect(a.x+b.x, a.y+b.y); }

// vector mapping to rectangle
Vect mapVect(Rect r, Vect a)
{
    float mapX, mapY;
    mapX = float((a.x+1)*r.horiz-1.0)/10.0;
    mapY = float((a.y+1)*r.vert-1.0)/10.0;
    mapX = mapX < 0.0 ? ceil(mapX-.5) : floor(mapX+.5);
    mapY = mapY < 0.0 ? ceil(mapY-.5) : floor(mapY+.5);
    return sumVect(Vect(int(mapX), int(mapY)), r.origin);
}

void makePict(SegmentsArray pic, Rect r)
{
    for (int i=0; i<pic.arraySize; ++i)
    {
        drawLine(   mapVect(r, pic[i].segStart),
                    mapVect(r, pic[i].segEnd));
    }
}

void rotatePict(SegmentsArray pic, Rect r)
{
    makePict(pic, Rect(Vect(r.origin.x+r.horiz, r.origin.y), r.vert, -r.horiz));
}

int main()
{
//    lineTest();
//    manualLineTest();
    SegmentsArray pict(3);
    pict[0] = LineSegment(Vect(0, 0), Vect(9, 0));
    pict[1] = LineSegment(Vect(0, 0), Vect(0, 9));
    pict[2] = LineSegment(Vect(7, 0), Vect(7, 5));
    Rect r1(Vect(30, 0), 45, 15);
    Rect r2(Vect(60, 20), 20, 10);

    system("CLS");
    makePict(pict, r1);
//    rotatePict(pict, r2);

    _getch();
    return 0;
}
