// drawline.h
#ifndef DRAWLINE_H_INCLUDED
#define DRAWLINE_H_INCLUDED

struct Console;
struct Vect
{
    int x, y;
    Vect(int xCord = 0, int yCord = 0) : x(xCord), y(yCord) {}
};
void drawPoint(int, int);
void drawLine(Vect, Vect);
void lineTest();
int readCoords(Vect&, Vect&);
void manualLineTest();

#endif // DRAWLINE_H_INCLUDED
