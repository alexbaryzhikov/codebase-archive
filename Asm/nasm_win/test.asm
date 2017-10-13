; Should be linked with gcc

    extern _printf
    global _WinMain@16

section .data

    mystring    db      '%d', 0xa, 0

section .bss
    num         resd    1

section .text

_WinMain@16:
    mov     eax, 0

    mov     eax, 0x60200004
    shl     eax, 16
    shr     eax, 16

    push    eax
    push    dword mystring
    call    _printf
    add     esp, byte 8
    
    ret     16
