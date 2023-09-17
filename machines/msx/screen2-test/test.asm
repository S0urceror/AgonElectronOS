    DEVICE NOSLOT64K
    PAGE 6

CHPUT       equ 00a2h
WRTVDP:		equ	#47
WRTVRM:		equ	#4d
CHGET:      equ #9f
INITXT:     equ #6c
H.KEYI      EQU 0FD9AH
RDVDP:		equ	#13e

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

    call SetVideoMode

    ld hl, 0x3800
    ld bc, 32*24
again0:    
    ld a, 0 ; index to pattern 0
    call WRTVRM
    inc hl
    dec bc
    ld a, c
    or b
    jr nz,again0

    ; set pattern 0
    ld b,8
    ld hl, 0x2000
    ld de, VDP_Pattern
again:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz again

    ; set pattern 0
    ld b,8
    ld hl, 0x2000+0x800*1
    ld de, VDP_Pattern+7
again3:
    ld a, (de)
    call WRTVRM
    dec de
    inc hl
    djnz again3

    ; set pattern 0
    ld b,8
    ld hl, 0x2000+0x800*2
    ld de, VDP_Pattern
again5:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz again5

    ld b,8
    ld hl, 0x0000
    ld de, VDP_Color
again2:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz again2

    ld b,8
    ld hl, 0x0000+0x800*1
    ld de, VDP_Color
again4:
    ld a, (de)
    xor 255
    call WRTVRM
    inc de
    inc hl
    djnz again4

    ld b,8
    ld hl, 0x0000+0x800*2
    ld de, VDP_Color
again6:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz again6    

sprites:
    ld b,8*4
    ld hl, 0x1800
    ld de, Sprite_Pattern
sprite_pattern:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz sprite_pattern

    ld b,4*2
    ld hl, 0x3b00
    ld de, Sprite_Attributes
sprite_atts:
    ld a, (de)
    call WRTVRM
    inc de
    inc hl
    djnz sprite_atts

    ; set start position
    ld a, 256/2 - 8
    ld (XPOS),a

    ; setup interrupt routine
    DI
    ld	a, 0C3h
    ld	(H.KEYI), a
    ld	hl, tickMain
    ld	(H.KEYI+1), hl	; Pone la rutina de interrupcion que lleva la logica del juego
    EI
    
forever:
    jr forever
    
tickMain:
    call	RDVDP		; clear interrupt flag
    ld a, (XPOS)
    ld hl, 0x3b00+1 ; x-coord
    inc a
    ld (XPOS),a
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

VDP_InitData:	
        db 2
		db 0E2h
		db 0Eh
		db 7Fh
		db 7
		db 76h
		db 3
		db 0E4h

VDP_Pattern:
    db 10000000b
    db 01000000b
    db 00100000b
    db 00010000b
    db 00001000b
    db 00000100b
    db 00000010b
    db 00000001b

VDP_Color:
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h
    db 0f0h

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
