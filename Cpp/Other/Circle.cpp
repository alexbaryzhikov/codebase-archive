//
//  Draw a line
//  Recommended console settings: raster font 8x8
//
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

#define RADIUS      10.0

void drawCircle(float x, float y, const float& dt)
{
    drawPoint(round(RADIUS*x)+80, round(RADIUS*y)+60);
    Sleep(1);
    drawCircle(x-y*dt, y+x*dt, dt);
}

int main()
{

    drawCircle(1.0, 0.0, 0.01);

    // logarithmic spiral
/*    float i=0.0, j=1.0, r;
    int x, y;
    for(;;)
    {
        for(;;)
        {
            r = exp(i);
            x = round(cos(i-j)*r)+80;
            y = round(sin(i-j)*r)+60;
            if(x>=0 && x<=160 && y>=0 && y<=120) drawPoint(x, y);
            else break;
            i += 0.02;
        }
        j += 0.1;
        i = 0;
        Sleep(100);
        system("CLS");
    }
*/
    _getch();
    return 0;
}
