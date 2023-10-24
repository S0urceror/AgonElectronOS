    DEVICE NOSLOT64K
    PAGE 5
    
CHPUT       equ 00a2h
CHGET		equ 009fh
WRTPSG	equ	00093h


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

	ld	b,13
PSGini:	
    ld	a,b	; 
	ld	e,0	; 8 least significant bits
	cp	7
	jr	nz,NoR7	; Jump if register different from 7
	ld	e,10111111b	; Bit 7 to 1 and bit 6 to 0
NoR7:	
    call	WRTPSG
	djnz	PSGini	; Loop to initialize registers 

	ld	a,0	; Register 0
	ld	e,0ach	; 8 least significant bits
	call	WRTPSG
	ld	a,1	; Register 1
	ld	e,1	; 4 most signifiant bits
	call	WRTPSG
	ld	a,6	; Register 6 - noise frequency
	ld	e,00011111b ; 3.608 Hz
	call	WRTPSG    
	ld	a,8	; Register 8
	ld	e,1100b	; Voice volume 1 to 12
	call	WRTPSG
	ld	a,7	; Register 7
	ld	e,10110111b	; Enable tone and noise
	call	WRTPSG
	;
	call CHGET ; keypress
	;
	ld	a,6	; Register 6 - noise frequency
	ld	e,00001111b ; 7.457 Hz
    call	WRTPSG    	
	ret

print:
    ld a,(hl)
    and a
    ret z
    inc hl
    call CHPUT
    jr print

strHello db "WRTPSG test",13,10,0
