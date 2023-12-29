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
T32NAM  equ     0F3BDH
T32COL  equ     0F3BFH
T32CGP  equ     0F3C1H
T32ATR  equ     0F3C3H
T32PAT  equ     0F3C5H

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

    ; set all entries in nametable to pattern 0
    ld hl, (T32NAM)
    ld bc, 32*24
    xor a
    call FILVRM

    ; set pattern
    ld bc,8
    ld de, (T32CGP)
    ld hl, VDP_Pattern
    call copytoVRAM

    ; set colours
    ld bc,8
    ld de, (T32COL)
    ld hl, VDP_Color
    call copytoVRAM

    ; set sprite 0 pattern
    ld bc,8*4
    ld de, (T32PAT)
    ld hl, Sprite_Pattern
    call copytoVRAM

    ; set sprite 0 attributes
    ld bc,4*2
    ld de, (T32ATR)
    ld hl, Sprite_Attributes
    call copytoVRAM

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
    ld hl, (T32ATR)
    inc hl ; x-coord
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
; T32NAM  equ     0F3BDH
; T32COL  equ     0F3BFH
; T32CGP  equ     0F3C1H
; T32ATR  equ     0F3C3H
; T32PAT  equ     0F3C5H

; T32NAM: defw    01800H
; T32COL: defw    02000H
; T32CGP: defw    00000H
; T32ATR: defw    01B00H
; T32PAT: defw    03800H
VDP_InitData:	
        db 000h ; 0 - mode 000
		db 0E2h ; 1 - screen 1, mode 000
		db 006h ; 2 - name table = 1800h
		db 080h ; 3 - colour table = 2000h
		db 000h ; 4 - pattern table = 0000h
		db 036h ; 5 - sprite attributes = 1b00h
		db 007h ; 6 - sprite patterns = 3800h
		db 0E4h ; 7 - gray on blue

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
    db 0e0h
    db 0d0h
    db 0c0h
    db 0b0h
    db 0a0h
    db 090h
    db 080h

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