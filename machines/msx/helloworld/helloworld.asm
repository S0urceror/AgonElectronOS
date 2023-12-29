    DEVICE NOSLOT64K
    PAGE 1
    
CHPUT equ 00a2h

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
    ret

print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print

strHello db "Hello MSX!",13,10,0

