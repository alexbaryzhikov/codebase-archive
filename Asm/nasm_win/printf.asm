; Should be linked with gcc

    extern _printf
    global _WinMain@16

section .data

    myint       dd      1234
    mystring    db      'This number -> %d <- should be 1234', 0xa, 0

section .text

_WinMain@16:            ;tell linker entry point
    mov     eax, 0

    push    dword [myint]
    push    dword mystring
    call    _printf
    add     esp, byte 8
    
    ret     16
