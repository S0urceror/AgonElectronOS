    DEVICE NOSLOT64K
    PAGE 5

CHPUT       equ 00a2h
WRTVDP:		equ	#47
WRTVRM:		equ	#4d
CHGET:      equ #9f
INITXT:     equ #6c
H.KEYI      EQU 0FD9AH
RDVDP:		equ	#13e
SETWRT:     equ #53
SNSMAT:		equ	#141	;  Read	keyboard row
FILVRM:     equ #56
CGTABL:  EQU     0004H
TXTNAM  equ     0F3B3H
TXTCGP  equ     0F3B7H

    org 4000h

    defb    "AB"
    defw    START
    defw    0
    defw    0
    defw    0
    defs    6

START:  
    ld hl, strHello
    call print

    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ;out     (055H),a

    ld bc,0
again:
    RST 28h
    DB 055h
    djnz again
    dec c
    jr nz, again
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

forever:
    jr forever

print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print


strHello db "IOPS!",13,10,0

    ORG 0x8000
XPOS db 0