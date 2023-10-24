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

; Copy from memory to VRAM
; HL = origin from memory
; DE = destination in VRAM
; BC = nr of bytes
copytoVRAM:
    push hl
    ld hl, de
    call SETWRT
    pop hl
copytoVRAMbyte:
    ld a, (hl)
    IFDEF AGONELECTRON
        RST 28h
	    DB 098h
    ELSE
        out ($98),a
    ENDIF
    ex af,af' ; store A
    inc hl
    dec bc
    ld a, c
    or b
    ret z
    ex af,af' ; restore A
    jr copytoVRAMbyte

START:  
    ld hl, strHello
    call print

    call SetVideoMode

    ; clear screen
    ld hl, (TXTNAM)
    ld bc, 40*24
    xor a
    call FILVRM

    ; copy font to patterntable
    ld bc, 8*256
    ld de, (TXTCGP)
    ld hl, (CGTABL)
    call copytoVRAM

    ld a, 'A'
    ld hl, (TXTNAM)
    call WRTVRM
    ld a, 'B'
    inc hl
    call WRTVRM

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

VDP_InitData:	
        db 000h ; 00000000
		db 0f0h ; 11110000
		db 000h ; name table
		db 000h ; colour table
		db 001h ; pattern table = 0x800
		db 000h ; sprite attributes
		db 000h ; sprite patterns
		db 0E4h ; white on blue

strHello db "Testing!",13,10,0

    ORG 0x8000
XPOS db 0