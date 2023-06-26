CHPUT equ 00a2h
HELLOHIGH EQU 0e000h

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC

    org 8000h
BEGIN:
START_BASIC:    
    ld hl, HELLOHIGH
    ld a, (hl)
    cp 'H'
    jr nz, _noprint
    call print  
_noprint:
    ld hl, strHello
    ld de, HELLOHIGH
    ld bc, ENDADR - strHello
    ldir
    ld hl, strHello
    call print
    DB 049h
    ret

print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print

strHello db "Hello MSX!",13,10,0

ENDADR: