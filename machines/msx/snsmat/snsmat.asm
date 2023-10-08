    DEVICE NOSLOT64K
    PAGE 5
    
CHPUT       equ 00a2h
SNSMAT:		equ	#141	;  Read	keyboard row


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

FOREVER:
    ld a, 8
    call SNSMAT
    cpl
    push af
    bit 4,a
    call nz, moveleft
    pop af
    push af
    bit 7,a
    call nz, moveright
    pop af
    push af
    bit 5,a
    call nz, moveup
    pop af
    push af
    bit 6,a
    call nz, movedown
    pop af
    bit 0,a
    call nz, space    
    jr FOREVER

moveleft:
    ld hl, strLeft
    call print
    ret
moveright:
    ld hl, strRight
    call print
    ret
moveup:
    ld hl, strUp
    call print
    ret
movedown:
    ld hl, strDown
    call print
    ret
space:
    ld hl, strSpace
    call print
    ret 
print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print

strHello db "SNSMAT test",13,10,0
strLeft db "left",13,10,0
strRight db "right",13,10,0
strUp db "up",13,10,0
strDown db "down",13,10,0
strSpace db "space",13,10,0