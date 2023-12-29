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

    org 4000h

    defb    "AB"
    defw    START
    defw    0
    defw    0
    defw    0
    defs    6

; Fill a contiguous area in VRAM
; A  = value to fill
; BC = nr of bytes
; assumes VRAM ptr has been set prior with SETWRT
; fillVRAM:
;     IFDEF AGONELECTRON
;         RST 28h
; 	    DB 098h
;     ELSE
;         out ($98),a
;     ENDIF
    
;     ex af,af' ; store A
;     dec bc
;     ld a, c
;     or b
;     ret z
;     ex af,af' ; restore A
;     jr fillVRAM

START:  
    ld hl, strHello
    call print

    call SetVideoMode

    ; set all entries in nametable to pattern 0
    ld hl, 0x3800 ; GRPNAM
    ld bc, 32*24
    xor a
    call FILVRM

    ; set pattern 0 in uppper 1/3th
    ld bc,8
    ld de, 0x2000
    ld hl, VDP_Pattern
    call LDIRVM
    ; set pattern 0 in middle 1/3th
    ld bc,8
    ld de, 0x2000+0x800*1
    ld hl, VDP_Pattern2
    call LDIRVM
    ; set pattern 0 in bottom 1/3th
    ld bc,8
    ld de, 0x2000+0x800*2
    ld hl, VDP_Pattern
    call LDIRVM

    ; set colours in upper 1/3th
    ld bc,8
    ld de, 0x0000
    ld hl, VDP_Color
    call LDIRVM
    ; set colours in middle 1/3th
    ld bc,8
    ld de, 0x0000+0x800*1
    ld hl, VDP_Color2
    call LDIRVM
    ; set colours in bottom 1/3th
    ld bc,8
    ld de, 0x0000+0x800*2
    ld hl, VDP_Color
    call LDIRVM

    ; set sprite 0 pattern
    ld bc,8*4
    ld de, 0x1800
    ld hl, Sprite_Pattern
    call LDIRVM

    ; set sprite 0 attributes
    ld bc,4*2
    ld de, 0x3b00
    ld hl, Sprite_Attributes
    call LDIRVM

    ; set start position
    ld a, 256/2 - 8
    ld (XPOS),a

    ; setup interrupt routine
    DI
    ld	hl, tickMain
    ld	(H.KEYI+1), hl	; Pone la rutina de interrupcion que lleva la logica del juego
    ld	a, 0C3h
    ld	(H.KEYI), a
    EI
    
forever:
    jr forever
    
tickMain:
    call	RDVDP		; clear interrupt flag
    ld a, 8
    call SNSMAT
    cpl
    bit 4,a
    ; update position
    ld a, (XPOS)
    ; flag still indicates bit 4
    jr nz, moveleft
    inc a
    jr move
moveleft:
    dec a
move:
    ld (XPOS),a
    ld hl, 0x3b00+1 ; x-coord
    call WRTVRM
    ret

print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print

;----------------------------------------------------
;
; Modo de video:
;
; Screen 2
; Sprites 16x16	unzoomed
; Pattern name table = #3800-#3AFF
; Pattern color	table =	#0000-#17FF
; Pattern generator table = #2000-#37FF
; Sprite atribute table	= #3b00-#3B7F
; Sprite generator table = #1800-#1FFF
; Background color = #E4 (Gris/Azul)
;----------------------------------------------------

SetVideoMode:
		ld	hl, VDP_InitData
		ld	d, 8
		ld	c, 0

setVideoMode2:
		ld	b, (hl)
		call	WRTVDP
		inc	hl
		inc	c
		dec	d
		jr	nz, setVideoMode2
		ret

; GRPNAM: defw    01800H
; GRPCOL: defw    02000H
; GRPCGP: defw    0
; GRPATR: defw    01B00H
; GRPPAT: defw    03800H
VDP_InitData:	
        db 002h
		db 0E2h ; mode 010
		db 00Eh ; 
		db 07Fh
		db 007h
		db 076h
		db 003h 
		db 0E4h ; gray on blue

VDP_Pattern:
    db 10000000b
    db 01000000b
    db 00100000b
    db 00010000b
    db 00001000b
    db 00000100b
    db 00000010b
    db 00000001b

VDP_Pattern2:
    db 00000001b
    db 00000010b
    db 00000100b
    db 00001000b
    db 00010000b
    db 00100000b
    db 01000000b
    db 10000000b
    
VDP_Color:
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
VDP_Color2:
    db 0fh
    db 0fh
    db 0fh
    db 0fh
    db 0fh
    db 0fh
    db 0fh
    db 0fh

Sprite_Attributes:
    db 192/2 - 8 
    db 256/2 - 8
    db 0
    db 6
    db 208
    db 0
    db 0 
    db 0

Sprite_Pattern:
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b    
    db 10101010b    

    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b
    db 10101010b    
    db 10101010b 

    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b

    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b
    db 11111111b
    db 00000000b
strHello db "Testing!",13,10,0

    ORG 0x8000
XPOS db 0