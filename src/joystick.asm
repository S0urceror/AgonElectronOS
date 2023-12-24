    INCLUDE	"macros.inc"
    INCLUDE	"equs.inc"
    INCLUDE "ez80f92.inc"

    .ASSUME	ADL = 1

    DEFINE .STARTUP, SPACE = ROM
    SEGMENT .STARTUP

    ; C functions
    XDEF    _init_joysticks
    XDEF    _read_joysticks

_init_joysticks:
    ; port C - all high impedence input
    ld a, 255
    out0 (PC_DDR), a
    xor a
    out0 (PC_ALT1), a
    out0 (PC_ALT2), a
    ; port D - pins 4 - 7 high impedence input, shared with UART
    in0 a,(PD_DDR)
    or  11110000b   ; set pin 4,5,6,7
    out0 (PD_DDR), a
    in0 a,(PD_ALT1)
    and 00001111b   ; reset pin 4,5,6,7
    out0 (PD_ALT1), a
    in0 a,(PD_ALT2)
    and 00001111b   ; reset pin 4,5,6,7
    out0 (PD_ALT2), a
    ret

; A
; PC1: Up
; PC3: Down
; PC5: Left
; PC7: Right
; PD5: Button 1
; PD7: Button 2
; B
; PC0: Up
; PC2: Down
; PC4: Left
; PC6: Right
; PD4: Button 1
; PD6: Button 2
_read_joysticks:
    push af
    ld hl,0
    in0 a, (PC_DR)
    ld l, a
    in0 a, (PD_DR)
    ld h, a
    pop af
    ret