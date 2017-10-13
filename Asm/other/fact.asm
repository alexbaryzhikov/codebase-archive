;=============================================================================
;
; Factorial
;
; Computes n! of the constant 'n', defined in the data section.
; Prints result in decimal form.
;
;=============================================================================

    SYS_EXIT    equ     1
    SYS_READ    equ     3
    SYS_WRITE   equ     4
    STDIN       equ     0
    STDOUT      equ     1

global  _start
global  _print_int

;-----------------------------------------------------------------------------
; Name:         print
; Implements:   Write to stdout.
; Arguments:    arg1 = len
;               ecx = address of first byte to print
;-----------------------------------------------------------------------------
%macro print 1
    mov     eax, SYS_WRITE
    mov     ebx, STDOUT
    mov     edx, %1
    int     0x80
%endmacro

segment .data

    n           dd      10 ; int32_t (-2,147,483,648..2,147,483,647)
    divisors    dd      1000000000, 100000000, 10000000, 1000000, 100000, 10000, 1000, 100, 10, 1
    errmsg      db      'Error: int32_t overflow.', 0xa
    errmsg_len  equ     $ - errmsg

segment .bss

section .text

_start:
    jmp     _main

;-----------------------------------------------------------------------------
; Name:         _print_int
; Implements:   Print the 32-bit number in decimal form.
;               Try test case n = 0xf00dbeef (-267534609).
; Arguments:    arg1 = number to print
;-----------------------------------------------------------------------------
_print_int:
    push    ebp
    mov     ebp, esp
    sub     esp, 8      ; allocate space for locals
    push    eax
    push    ebx
    push    ecx
    push    edx
    push    esi

    ;--------------------------------------------------
    ; Local variables:
    ; dword  remainder   = [ebp-4]
    ; word   output      = [ebp-6]
    ; byte   sign        = [ebp-7]

    mov     eax, [ebp+8]
    mov     esi, divisors
    mov     byte [ebp-7], 0

    ; deal with the sign
    cmp     eax, 0
    jl      _L0                 ; it's negative, handle the problem

    mov     dword [ebp-4], eax  ; it's positive, just store it and move on
    jmp     _L1

_L0:
    mov     byte [ebp-7], 1     ; one will be added to the last digit
    not     eax                 ; inverse bits
    push    eax

    ; print '-'
    mov     byte [ebp-6], '-'
    mov     ecx, ebp
    sub     ecx, 6
    print   1

    pop     eax

_L1:
    ; skip leading zeros
    mov     ebx, [esi]
    cmp     eax, ebx
    jge     _L2         ; exit loop if nonzero found

    add     esi, 4      ; increment pointer
    mov     ebx, [esi]
    cmp     ebx, 1      ; loop while divisor > 1
    jne     _L1
    jmp     _L3         ; one-digit number, just print the last digit

_L2:
    ; get current digit and remainder
    xor     edx, edx
    div     ebx
    mov     dword [ebp-4], edx
    add     eax, '0'
    mov     byte [ebp-6], al
    mov     ecx, ebp
    sub     ecx, 6
    print   1

    add     esi, 4      ; increment pointer
    mov     ebx, [esi]  ; load next divisor
    cmp     ebx, 1      ; if divisor == 1 ...
    je      _L3         ; ... break the loop

    mov     eax, [ebp-4]  ; load remainder
    jmp     _L2

_L3:
    ; print last digit
    mov     eax, [ebp-4]
    add     al, [ebp-7]
    add     eax, '0'
    mov     byte [ebp-6], al
    mov     byte [ebp-5], 0xa ; append newline
    mov     ecx, ebp
    sub     ecx, 6
    print   2

    pop     esi
    pop     edx
    pop     ecx
    pop     ebx
    pop     eax
    leave       ; mov esp,ebp / pop ebp
    ret

;-----------------------------------------------------------------------------
; Name:         _main
; Implements:   Compute and print n!.
;-----------------------------------------------------------------------------
_main:
    mov     eax, 1
    mov     ecx, [n]
    xor     edx, edx
_L4:
    mul     ecx
    test    edx, edx
    jnz     _L5
    loop    _L4

    push    eax         ; push argument
    call    _print_int
    add     esp, 4      ; deallocate argument space

_end:
    mov     eax, SYS_EXIT
    int     0x80

_L5:
    mov     ecx, errmsg
    print   errmsg_len
    jmp     _end
