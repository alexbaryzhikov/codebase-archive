    SYS_EXIT    equ     1
    SYS_READ    equ     3
    SYS_WRITE   equ     4
    STDIN       equ     0
    STDOUT      equ     1

;-----------------------------------------------------------------------------
; Name:        print
; Implements:  write to stdout system call
; Params:      str, len
;-----------------------------------------------------------------------------
%macro print 2
    mov     eax, SYS_WRITE
    mov     ebx, STDOUT
    mov     ecx, %1
    mov     edx, %2
    int     0x80
%endmacro

segment .data

    n           dw      0xF00D ; int16_t test number (-32768..32767)
    divisors    dw      10000, 1000, 100, 10, 1

segment .bss

    rem         resw    1
    outc        resb    2
    sign        resb    1

section .text
    global _start

;-----------------------------------------------------------------------------
; Name:        main
; Implements:  print the 16-bit constant 'n' in decimal form
;-----------------------------------------------------------------------------
_start:
    mov     ax, [n]
    mov     esi, divisors
    xor     bl, bl
    mov     [sign], bl

    ; deal with the sign
    cmp     ax, 0
    jl      _L0         ; it's negative, handle the problem

    mov     [rem], ax   ; it's positive, just store it and move on
    jmp     _L1

_L0:
    mov     bl, 1
    mov     [sign], bl  ; one will be added to the last digit
    xor     ax, -1      ; inverse bits
    mov     [rem], ax

    ; print '-'
    mov     bl, '-'
    mov     [outc], bl
    print   outc, 1
   
    mov     ax, [rem]   ; restore ax

_L1:
    ; skip leading zeros
    mov     bx, [esi]
    cmp     ax, bx
    jge     _L2         ; exit loop if nonzero found

    add     esi, 2      ; increment pointer
    mov     bx, [esi]  
    cmp     bl, 1       ; while rank > 1 ...
    jne     _L1         ; ... continue skipping
    jmp     _L3         ; one-digit number, just print last digit

_L2:
    ; get rank digit and remainder
    xor     edx, edx
    div     bx
    mov     [rem], dx
    add     al, '0'
    mov     [outc], al

    ; print digit
    print outc, 1

    add     esi, 2      ; increment pointer
    mov     bx, [esi]   ; load next divisor
    cmp     bl, 1       ; if divisor == 1 ...
    je      _L3         ; ... break the loop

    mov     ax, [rem]   ; load remainder
    jmp     _L2

_L3:
    ; print last digit
    mov     ax, [rem]
    add     ax, [sign]
    add     ax, '0'
    mov     [outc], al
    mov     al, 0xa
    mov     [outc+1], al ; append newline
    print   outc, 2

    mov     eax, SYS_EXIT
    int     0x80
