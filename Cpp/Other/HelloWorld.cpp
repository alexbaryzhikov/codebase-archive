#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <io.h>
#include <fcntl.h>

int main() {
    _setmode(_fileno(stdout), _O_U16TEXT);
    wchar_t a[] = { L'\u2665', L'\u2666', L'\u2663', L'\u2660' };
    std::wcout << a[0] << a[1] << a[2] << a[3];
    _setmode(_fileno(stdout), _O_TEXT);
    return 0;
}
