#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <windows.h>
using namespace std;

void showCursor(bool showFlag)
{
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);

    CONSOLE_CURSOR_INFO     cursorInfo;

    GetConsoleCursorInfo(out, &cursorInfo);
    cursorInfo.bVisible = showFlag; // set the cursor visibility
    SetConsoleCursorInfo(out, &cursorInfo);
}

void showWinnerMessage()
{
    const char winMsg[] = " WINNER WINNER CHICKEN DINNER! ";
    HANDLE hstdout = GetStdHandle( STD_OUTPUT_HANDLE );
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    GetConsoleScreenBufferInfo( hstdout, &csbi );
    COORD curPos = csbi.dwCursorPosition;;
    showCursor(false);
    //int outColor[] = { 7, 9, 11, 10, 12, 14, 13 };
    int outColor[] = { 8, 2, 3, 6, 4, 5 };
    //int outColor[] = { 8, 7, 2, 9, 3, 11, 6, 10, 4, 12, 14, 5, 13 };
    int i = 0;
    for( ;; )
    {
        curPos.X = 0;
        SetConsoleCursorPosition( hstdout, curPos );
        SetConsoleTextAttribute( hstdout, outColor[i] * 16 + 0xf );
        cout << winMsg;
        if( ++i == (sizeof(outColor) / sizeof(outColor[0])) ) i = 0;
        Sleep(100);
        if( GetAsyncKeyState( VK_RETURN ) ) break;
    }
    cout << endl;
    SetConsoleTextAttribute( hstdout, csbi.wAttributes );
    showCursor(true);
}

int main()
{
    cout << "\n";
    showWinnerMessage();
	return 0;
}
