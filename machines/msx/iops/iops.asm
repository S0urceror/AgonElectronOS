    DEVICE NOSLOT64K
    PAGE 1

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
LDIRVM:     equ #5c
LDIRMV:     equ #59
CGTABL:  EQU     0004H
TXTNAM  equ     0F3B3H
TXTCGP  equ     0F3B7H

UART0_SPR EQU 0c7h

    MACRO OUT0_A address
        ; out (address),a
        DB 0EDh
        DB 039h
        DB address
    ENDM
    MACRO IN0_A address
        ;in a,(address)
        DB 0EDh
        DB 038h
        DB address
    ENDM

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

    ld hl, strFILVRM
    call print
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl,0
    ld bc, 0ffffh
    ld a, 0
    call FILVRM

    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, strLDIRVM
    call print
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, 00000h
    ld de, 00000h
    ld bc, 0ffffh
    call LDIRVM

    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, strLDIRMV
    call print
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, 0000h
    ld de, 8000h
    ld bc, 10h
    call LDIRMV

    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, strOUTput55h
    call print
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ; 65536 times we cycle
    ld bc,0
out_again:
    ; OUT (0x55),A
    ; is an instruction that is encoded in two bytes machine code
    ; we replace this by RST28h with the port in the following byte and A containing the value
    ; this makes a byte-wise exact fit to replace OUTs in existing MSX code and games
    ;
    ; For this test we use a port that does nothing on the MSX but is used to 
    ; simulate the maximum speed we can OUTput bytes to the ESP32 from the EZ80
    ; remember that one OUT translates to multiple bytes on the wire
    ; because we need to transmit the following things:
    ; * if we do an IN or an OUT
    ; * the port we are addressing
    ; * the value that is output or input
    RST 28h
    DB 055h
    djnz out_again
    dec c
    jr nz, out_again
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, strOUTput98h
    call print

    ; print clock
    DB 0x5b ; .LIL
    rst 30h
    ; 65536 times we cycle
    ld bc,0
out_vdp_again:
    ; OUT (0x98),A
    ; is an instruction that is encoded in two bytes machine code
    ; we replace this by RST28h with the port in the following byte and A containing the value
    ; this makes a byte-wise exact fit to replace OUTs in existing MSX code and games
    ;
    ; For this test we use a port that does nothing on the MSX but is used to 
    ; simulate the maximum speed we can OUTput bytes to the ESP32 from the EZ80
    ; remember that one OUT translates to multiple bytes on the wire
    ; because we need to transmit the following things:
    ; * if we do an IN or an OUT
    ; * the port we are addressing
    ; * the value that is output or input
    RST 28h
    DB 098h
    djnz out_vdp_again
    dec c
    jr nz, out_vdp_again
    
    ; print clock
    DB 0x5b ; .LIL
    rst 30h

    ld hl, strINput98h
    call print

    ; print clock
    DB 0x5b ; .LIL
    rst 30h
    ; 65536 times we cycle
    ld bc,0
in_again:
    ; IN A,(0x55)
    ; is an instruction that is encoded in two bytes machine code
    ; we replace this by RST38h with the port in the following byte and A containing the value
    ; this makes a byte-wise exact fit to replace INs in existing MSX code and games
    ;
    ; For this test we use a port that does nothing on the MSX but is used to 
    ; simulate the maximum speed we can INput bytes from the ESP32 to the EZ80
    ; remember that one IN translates to multiple bytes on the wire
    ; because we need to transmit the following things:
    ; * if we do an IN or an OUT
    ; * the port we are addressing
    ; * the value that is output or input
    RST 38h
    DB 055h
    djnz in_again
    dec c
    jr nz, in_again

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
strOUTput55h db "OUT 55h",13,10,0
strOUTput98h db "OUT 98h - vdp write vram",13,10,0
strINput98h db "IN 98h - vdp read vram",13,10,0
strFILVRM db "OUT 98h - fillvrm",13,10,0
strLDIRVM db "OUT 98h - ldirvm, vram from memory",13,10,0
strLDIRMV db "OUT 98h - ldirmv, memory from vram",13,10,0
    ORG 0x8000
XPOS db 0