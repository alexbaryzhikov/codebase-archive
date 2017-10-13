/*
Draw a line

Recommended console settings: raster font 8x8, buffer size 160x120
*/
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <windows.h>
#include <conio.h>
#include <math.h>
using namespace std;

// setup console
struct Console
{
    HANDLE                      hstdout;
    CONSOLE_SCREEN_BUFFER_INFO  csbi;
    CONSOLE_CURSOR_INFO         cursorInfo;

    Console(unsigned width, unsigned height)
    {
        SMALL_RECT r;
        COORD      c;
        hstdout = GetStdHandle(STD_OUTPUT_HANDLE);
        GetConsoleScreenBufferInfo(hstdout, &csbi);
        r.Left   =
        r.Top    = 0;
        r.Right  = width -1;
        r.Bottom = height -1;
        SetConsoleWindowInfo(hstdout, TRUE, &r);
        c.X = width;
        c.Y = height;
        SetConsoleScreenBufferSize(hstdout, c);
    }
    ~Console()
    {
        SetConsoleTextAttribute(hstdout, csbi.wAttributes);
        SetConsoleScreenBufferSize(hstdout, csbi.dwSize);
        SetConsoleWindowInfo(hstdout, TRUE, &csbi.srWindow);
    }
    void color(WORD color = 0x07)
    {
        SetConsoleTextAttribute(hstdout, color);
    }
    void showCursor(bool showFlag)
    {
        GetConsoleCursorInfo(hstdout, &cursorInfo);
        cursorInfo.bVisible = showFlag;  // set the cursor visibility
        SetConsoleCursorInfo(hstdout, &cursorInfo);
    }
};

Console con(160, 120);

struct CoordsXY
{
    int x, y;
    CoordsXY(int inX = 0, int inY = 0) : x(inX), y(inY) {}
};

void drawPoint(int x, int y)
{
    COORD       pos = { x, y };
    HANDLE      hstdout = GetStdHandle(STD_OUTPUT_HANDLE);
    SetConsoleCursorPosition(hstdout, pos);
    //cout << (char)219;
    cout << (char)254;
}

//#define FLOAT_CALC_METHOD
#ifdef FLOAT_CALC_METHOD
// draw the line with floating point calculations
void drawLine(CoordsXY a, CoordsXY b)
{
    int     x0 = a.x, y0 = a.y,
            dx = abs(b.x - a.x), stepX = a.x < b.x ? 1 : -1,
            dy = abs(b.y - a.y), stepY = a.y < b.y ? 1 : -1;
    float   slope = dx==0 ? 9999999.0 : (float)dy/(float)dx,
            error = slope-1, err2;
    for(;;)
    {
        drawPoint(x0, y0);
        if(x0==b.x && y0==b.y) break;
        err2 = 2*error;
        if(err2 <= slope)   { error += slope;   x0 += stepX; }
        if(err2 >= -1)      { error += -1;      y0 += stepY; }
    }
}
#else
// draw the line with integer calculations
void drawLine(CoordsXY a, CoordsXY b)
{
    int x0=a.x, y0=a.y,
        x1=b.x, y1=b.y;
    int dx =  abs(x1-x0), stepX = x0<x1 ? 1 : -1;
    int dy = -abs(y1-y0), stepY = y0<y1 ? 1 : -1;
    int error = dx+dy, err2;  // error value e_xy
    for(;;)
    {
        drawPoint(x0, y0);
        if(x0 == x1 && y0 == y1) break;
        err2 = error*2;
        if(err2 >= dy) { error += dy; x0 += stepX; } // e_xy + e_y >= 0
        if(err2 <= dx) { error += dx; y0 += stepY; } // e_xy - e_x <= 0
    }
}
#endif // FLOAT_CALC_METHOD
int readCoords(CoordsXY& a, CoordsXY& b)
{
        cout << "> enter coords of A and B\n> ";
        scanf("%d", &a.x);
        if(a.x < 0) return 66;
        scanf("%d", &a.y);
        cout << "> enter coords of B\n> ";
        scanf("%d%d", &b.x, &b.y);
        fflush(stdin);
        return 0;
}

void lineTest()
{
    // test circle
    #define RADIUS      50
    #define OFFSET_X    80
    #define OFFSET_Y    60
    #define ANGLE       15
    #define DEG         0.01745329252
    system("CLS");
    con.showCursor(false);
    CoordsXY    testPnt[360/ANGLE],
                testCent(OFFSET_X, OFFSET_Y);
    int         color = 0xa;
    for(int i = 0; i < 360/ANGLE; ++i)
    {
        testPnt[i].x = round(cos(i * ANGLE * DEG) * RADIUS + OFFSET_X);
        testPnt[i].y = round(sin(i * ANGLE * DEG) * RADIUS + OFFSET_Y);
        con.color(color);
        drawLine(testCent, testPnt[i]);
        if(color++ == 0xf) color = 0xa;  // cycle colors 0xa..0xf
        _getch();
    }
    con.color(7);
}

int main()
{
    lineTest();
    // manual coordinates input
    CoordsXY ptA, ptB;
    for(;;)
    {
        system("CLS");
        con.showCursor(true);
        if(readCoords(ptA, ptB) == 66) break;
        con.showCursor(false);
        system("CLS");
        drawLine(ptA, ptB);
        _getch();
    }
    return 0;
}
