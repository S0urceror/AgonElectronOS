;   .ASSUME ADL=0

;       symbols used to procedure alternate code

;       INTHZ   interrupt frequency
;       CHRGEN  character generator
;               0 = japanese
;               1 = international
;               2 = USSR ??
;       DATFMT  date format
;               0 = Y-M-D
;               1 = M-D-Y
;               2 = D-M-Y
;       KEYTYP  keyboard layout
;               0 = Japanese 
;               1 = International (QWERTY/other) 
;               2 = French (AZERTY) 
;               3 = English 
;               4 = German (DIN)
;               5 = USSR
;               6 = Spanish
;               7 = Swedish ??
;       BASVER  0 = Japanese
;               1 = International

INTHZ EQU 60
CHRGEN EQU 1
DATFMT EQU 1
KEYTYP EQU 1
BASVER EQU 1
MSXVER EQU 0 ; MSX1

VARWRK  EQU     0F380H
H.KEYI  EQU     0FD9AH
HIMEM   EQU     0FC4AH
BOTTOM  EQU     0FC48H
PRMSTK  EQU     0F6E4H
PRMPRV  EQU     0F74CH
STKTOP  EQU     0F674H
MEMSIZ  EQU     0f672h
NULBUF  EQU     0f862h
FCERR   EQU     0475AH
QUETAB  EQU     0F959H
KEYBUF  EQU     0FBF0H
H.RUNC  EQU     0FECBh
H.PHYD  EQU     0FFA7h

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC

    .ORG 0000h
BEGIN:
START_BASIC:

BOOT:                   ; 0000h
    JP _BOOT
    DS 000Ch - $
RDSLT:                  ; 000Ch
    JP _RDSLT
    DS 0010h - $
CHRGTR:                 ; RST 10h
    RET
    DS 0014h - $
WRSLT:                  ; 0014h
    JP _WRSLT
    DS 0018h - $
OUTDO:                   ; RST 18h
    JP CHPUT
    DS 001Ch - $
CALSLT:                 ; 001Ch
    JP _CALSLT
    DS 0020h - $
DCOMPR:                 ; RST 20h
    JP _DCOMPR
    DS 0024h - $
ENASLT:                 ; 0024h
    JP _ENASLT
    DS 0028h - $
GETTYPR:                ; RST 28h
    JP _WRITE_PORT
    DS 002Bh - $
IDBYT0:
    IF      INTHZ EQ 60
    DEFB    CHRGEN+16*DATFMT
    ELSE
    DEFB    CHRGEN+16*DATFMT+128
    ENDIF
IDBYT1: DEFB    KEYTYP+16*BASVER
IDBYT2: DEFB    MSXVER                  ; MSX version 0 = MSX1
    DEFB    0
    DS 0030h - $
CALLF:                  ; RST 30h
    JP _CALLF
    DS    00034H-$
; The next bytes are used by the diskrom, to initialize the double byte header char
; table (0F30FH). I have not seen a MSX with anything other than four zero's, meaning
; no double byte chars.
D0034:  db    0,0
        db    0,0    
    DS 0038h - $
KEYINT:                 ; RST 38h
    JP _READ_PORT

    DS 009ch - $
CHSNS:
    JP _CHSNS

    DS 009fh - $
CHGET:                  ; 009fh
    JP _CHGET
    
    DS 00a2h - $
CHPUT:                  ; 00a2h
    DB 0x5b ; .LIL
    RST 08h
    RET

    DS 00b7h - $
BREAKX:
    and a ; no CTRL-STOP pressed
    RET

    DS 0141h - $
SNSMAT:                 ; 0141h
    JP _SNSMAT

    DS 0144h - $
PHYDIO:                 ; 0144h
    JP _PHYDIO

    DS 0159h - $
CALBAS:                 ; 0159h
    RET

_BOOT:
    ; assume only primary slots no secundary slots
    ; assume that 0000-3FFF is in slot 00 - ROM
    ; assume that 4000-7FFF is in slot 00 - ROM
    ; assume that 8000-BFFF is in slot 01 - RAM
    ; assume that C000-FFFF is in slot 01 - RAM
    ld a, 0b01010100
    ld ix, 0x0004
    scf ; set carry = write
    DB 0x5b ; .LIL
    rst 38h

    ; setup MSX variables in page 3
    ld      bc,0C49H
    ld      de,VARWRK+1
    ld      hl,VARWRK
    ld      (hl),0
    ldir                            ; clear system variable area
    LD      BC,00090H
    LD      DE,VARWRK
    LD      HL,I7F27
    LDIR                            ; initialize some systemvariables
    LD      BC,00230H-1
    LD      DE,H.KEYI+1
    LD      HL,H.KEYI
    LD      (HL),0C9H
    LDIR                            ; initialize hooks
    LD      HL,VARWRK
    LD      (HIMEM),HL              ; highest BASIC RAM address
    LD      HL,08000h
    LD      (BOTTOM),HL             ; lowest BASIC RAM address
    ld      hl,0f168h
    ld      (MEMSIZ),hl
    ld      hl,0f177h
    ld      (NULBUF),hl    
    LD      HL,0f6e4h
    LD      (PRMPRV),HL             ; initialize previous FN block pointer
    LD      HL,0f0a0h
    LD      (STKTOP),HL             ; Z80 stack temporary at PRMSTK
    LD      SP,HL
    
    ; display MSX boot prompt
    ld hl, MSX_INTRO
    call _print
    ld      b,010h
A7D0D:  
    dec     hl
    ld      a,l
    or      h
    jr      nz,A7D0D
    djnz    A7D0D                   ; wait 3 seconds

    ; get contents of diskrom start address in slot 1
    ld ix,0
    ld a, 1
    ld hl, 04002h
    call _RDSLT
    ld ixl, a
    ld a, 1
    ld hl, 04003h
    call _RDSLT
    ld ixh, a
    ; jump there
    ld iy, 0100h
    call CALSLT

    ; emulate BASIC continuing init and then calling H.RUNC
    call H.RUNC
    ; diskrom should now have attempted to run MSXDOS
    ; if there is no disk mounted or not a valid disk we return here
    EI
    ld hl, FAKE_BASIC_PROMPT
    call _print
_forever:
    jr _forever

_print:
_next_character:
    ld a, (hl)
    and a
    ret z
    DB 0x5b ; .LIL
    RST 08h
    inc hl
    jr _next_character

MSX_INTRO:
    DB 12,"\r\n\r\n\r\n",9,9,9,9,"MSX  system\r\n",9,9,9,9,"version 1.0\r\n",9,9,"Copyright 1983 by Microsoft\r\n\r\n\r\n",0
FAKE_BASIC_PROMPT:
    DB "MSX BASIC version 5.0\r\nCopyright 2023 by S0urceror\r\n65535 Bytes free\r\nDisk BASIC version 1.0\r\nOk\r\n"

_DCOMPR:
;       Subroutine      DCOMPR
;       Inputs          ________________________
;       Outputs         ________________________
    ld      a,h
    sub     d
    ret     nz
    ld      a,l
    sub     e
    ret

_CHGET:
    DB 0x5b ; .LIL
    RST 10h
    jr c, _CHGET
    RET

_CHSNS
    DB 0x5b ; .LIL
    RST 10h
    and a
    RET

_SNSMAT:
    push bc
    ld b,a ; preserve matrix line nr.
    ;
    ; read character from fifo
    DB 0x5b ; .LIL
    RST 10h
    jr c, HANDLE_SNSMAT_NOPRESS
    
    ; TODO check ASCII with line-nr
    ld a, 0xff ; no presses for now
    pop bc
    RET

HANDLE_SNSMAT_NOPRESS:
    ld a, 0xff
    pop bc
    RET

_READ_PORT:
    POP IY
    INC IY
    PUSH IY
    LD A,(IY-1)
    cp 0xa8 
    jr z,_READ_A8 
    ; any other port
    xor a
    RET
_READ_A8:
    push ix
    ld ix, 0x0004
    and a ; reset carry = read
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    RET

_WRITE_PORT:
    POP IY      ; get address of following instruction
    INC IY
    PUSH IY     ; return address is next byte
    push af
    LD A,(IY-1)
    cp 0xa8 
    jr z,_WRITE_A8 
    ; no other OUT's
    pop af
    RET
_WRITE_A8:
    push ix
    ld ix, 0x0004
    scf ; set carry = write
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    pop af
    RET

_RDSLT:
    push ix
    ld ix, 0x0006
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret
    
_WRSLT:
    push ix
    ld ix, 0x0008
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret

_CALLF:
    ex      (sp),hl
    push    af
    push    de
    ld      a,(hl)
    push    af
    pop     iy                      ; slotid
    inc     hl
    ld      e,(hl)
    inc     hl
    ld      d,(hl)
    inc     hl
    push    de
    pop     ix                      ; adres
    pop     de
    pop     af
    ex      (sp),hl
    ; now slotid is in IY and address is in IX
_CALSLT:
    ; first save IX
    DB 0x5b ; .LIL
    rst 18h
    ;  electron os calslt
    ld ix, 0x000a
    DB 0x5b ; .LIL
    rst 38h
    ret

_ENASLT:
    push ix
    ld ix, 0x000c
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret

_PHYDIO:
	CALL    H.PHYD
    RET

I7F27: ; area that will be copied to F380, contains code and initialisation
    .PHASE  VARWRK

RDPRIM: RST     28h     ; MSM: replace out (a8h),a by RST28
        DB      0a8h
        ;OUT     (0A8H),A
        LD      E,(HL)
        JR      J7F2F

WRPRIM: RST     28h     ; MSM: replace out (a8h),a by RST28
        DB      0a8h
        ;OUT     (0A8H),A
        LD      (HL),E
J7F2F:  LD      A,D
        RST     28h     ; MSM: replace out (a8h),a by RST28
        DB      0a8h
        ;OUT     (0A8H),A
        RET

CLPRIM: RST     28h     ; MSM: replace out (a8h),a by RST28
        DB      0a8h
        ;OUT     (0A8H),A
        EX      AF,AF'
        CALL    CLPRM1
        EX      AF,AF'
        POP     AF
        RST     28h     ; MSM: replace out (a8h),a by RST28
        DB      0a8h
        ;OUT     (0A8H),A
        EX      AF,AF'
        RET

CLPRM1: JP      (IX)

USRTAB: defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call
        defw    FCERR                   ; illegal function call

LINL40:
        defb    39

LINL32: defb    29

LINLEN:
        defb    29

CRTCNT: defb    24

CLMLST: defb    14

TXTNAM: defw    0
TXTCOL: defw    0
TXTCGP: defw    00800H
TXTATR: defw    0
TXTPAT: defw    0

T32NAM: defw    01800H
T32COL: defw    02000H
T32CGP: defw    0
T32ATR: defw    01B00H
T32PAT: defw    03800H

GRPNAM: defw    01800H
GRPCOL: defw    02000H
GRPCGP: defw    0
GRPATR: defw    01B00H
GRPPAT: defw    03800H

MLTNAM: defw    00800H
MLTCOL: defw    0
MLTCGP: defw    0
MLTATR: defw    01B00H
MLTPAT: defw    03800H

CLIKSW: defb    1
CSRY:   defb    1
CSRX:   defb    1
CNSDFG: defb    0

RG0SAV: defb    000H
RG1SAV: defb    0E0H
RG2SAV: defb    000H
RG3SAV: defb    000H
RG4SAV: defb    000H
RG5SAV: defb    000H
RG6SAV: defb    000H
RG7SAV: defb    000H
STATFL: defb    000H
TRGFLG: defb    0FFH
FORCLR: defb    15
BAKCLR: defb    4

BDRCLR:
        defb    7

MAXUPD: jp      0
MINUPD: jp      0
ATRBYT: defb    15
QUEUES: defw    QUETAB
FRCNEW: defb    0FFH
SCNCNT: defb    1
REPCNT: defb    50
PUTPNT: defw    KEYBUF
GETPNT: defw    KEYBUF
CS1200: defb    053H,05CH,026H,02DH,00FH
CS2400: defb    025H,02DH,00EH,016H,01FH
        defb    053H,05CH
        defb    026H,02DH
        defb    00FH
ASPCT1: defw    00100H
ASPCT2: defw    00100H
ENDPRG: defb    ':'

        .DEPHASE
ENDADR:
