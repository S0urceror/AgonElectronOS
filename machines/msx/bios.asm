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
H.TIMI  EQU	    0FD9Fh
H.KEYI  EQU     0FD9AH

RG0SAV  EQU     0F3DFH

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC

    .ORG 0000h
BEGIN:
START_BASIC:

BOOT:                   ; 0000h
    JP _BOOT
    DW CGTABL           ; 0004h Location MSX font in ROM
    DB 0x98             ; 0006h VDP READ PORT
    DB 0x98             ; 0007h VDP WRITE PORT
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
OUTDO:                  ; RST 18h
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

    DS 0047h - $
WRTVDP:
    JP _WRTVDP    

    DS 004Ah - $
RDVRM:
    JP _RDVRM  

    DS 004Dh - $
WRTVRM:
    JP _WRTVRM  

    DS 0050h - $
SETRD:
    JP _SETRD  

    DS 0053h - $
SETWRT:
    JP _SETWRT

    DS 0093h - $
WRTPSG:
    JP _WRTPSG

    DS 0096h - $
RDPSG:
    JP _RDPSG

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

    DS 013eh - $
RDVDP:
    JP _RDVDP

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
    
    ; set vsync address
    ld ix, 0x000e
    ld hl, H.KEYI
    DB 0x5b ; .LIL
    rst 38h
    ;

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
    DB 12,"\r\n\r\n\r\n"
    DB "              MSX  system\r\n"
    DB "              version 1.0\r\n"
    DB "      Copyright 1983 by Microsoft\r\n"
    DB "\r\n\r\n",0
FAKE_BASIC_PROMPT:
    DB "MSX BASIC version 5.0\r\n"
    DB "Copyright 2023 by S0urceror\r\n"
    DB "65535 Bytes free\r\n"
    DB "Disk BASIC version 1.0\r\n"
    DB "Ok\r\n"

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

;Address  : #009F
;Function : One character input (waiting)
;Output   : A  - ASCII code of the input character
;Registers: AF
_CHGET:
    DB 0x5b ; .LIL
    RST 10h
    jr c, _CHGET
    RET

;Address  : #009C
;Function : Tests the status of the keyboard buffer
;Output   : Zero flag set if buffer is empty, otherwise not set
;Registers: AF
_CHSNS
    DB 0x5b ; .LIL
    RST 10h
    and a
    RET

;Address  : #0141
;Function : Returns the value of the specified line from the keyboard matrix
;Input    : A  - For the specified line
;Output   : A  - For data (the bit corresponding to the pressed key will be 0)
;Registers: AF
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
    ld iyl,a
    and a ; reset carry = read
    DB 0x5b ; .LIL
    rst 28h
    ret

_WRITE_PORT:
    POP IY      ; get address of following instruction
    INC IY
    PUSH IY     ; return address is next byte
    push af     ; store A to stack
    LD A,(IY-1) ; port number we want to write
    ld iyl,a    ; port in IYl
    pop af      ; restore original A
    ld iyh,a    ; value in IYh
    scf         ; set carry = write
    DB 0x5b ; .LIL
    rst 28h
    ret

;Address  : #000C
;Function : Reads the value of an address in another slot
;Input    : A  - ExxxSSPP  Slot-ID
;           │        ││└┴─ Primary slot number (00-11)
;           │        └┴─── Secondary slot number (00-11)
;           └───────────── Expanded slot (0 = no, 1 = yes)
;           HL - Address to read
;Output   : A  - Contains the value of the read address
;Registers: AF, C, DE
;Remark   : This routine turns off the interupt, but won't turn it on again
_RDSLT:
    push ix
    ld ix, 0x0006
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret
;Address  : #0014
;Function : Writes a value to an address in another slot.
;Input    : A  - Slot ID, see RDSLT
;           HL - Address
;           E  - Value
;Registers: AF, BC, D
;Remark   : See RDSLT    
_WRSLT:
    push ix
    ld ix, 0x0008
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret
;Address  : #0030
;Function : Executes an interslot call
;Output   : Depends on the calling routine
;Registers: AF, and the other registers depending on the calling routine
;Remark   : The following is the calling sequence:
;           RST #30
;           DB destination slot ID, see RDSLT
;           DW destination address
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

;Address  : #001C
;Function : Executes inter-slot call.
;Input    : IY - High byte with slot ID, see RDSLT
;           IX - The address that will be called
;Remark   : Variables can never be given in alternative registers or IX and IY
_CALSLT:
    ; first save IX
    DB 0x5b ; .LIL
    rst 18h
    ;  electron os calslt
    ld ix, 0x000a
    DB 0x5b ; .LIL
    rst 38h
    ret

;Address  : #0024
;Function : Switches indicated slot at indicated page on perpetually
;Input    : A - Slot ID, see RDSLT
;           H - Bit 6 and 7 must contain the page number (00-11)
_ENASLT:
    push ix
    ld ix, 0x000c
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret
    
;Address  : #0144
;Function : Executes I/O for mass-storage media like disks
;Input    : F  - Set carry to write, reset carry to read
;           A  - Drive number (0 = A:, 1 = B:, etc.)
;           B  - Number of sectors
;           C  - Media ID of the disk
;           DE - Begin sector
;           HL - Begin address in memory
;Output   : F  - Carry set on error
;           A  - Error code (only if carry set)
;                0 = Write protected
;                2 = Not ready
;                4 = Data error
;                6 = Seek error
;                8 = Record not found
;                10 = Write error
;                12 = Bad parameter
;                14 = Out of memory
;                16 = Other error
;           B  - Number of sectors actually written or read
;Registers: All
;Remark   : Interrupts may be disabled afterwards. On some hard disk interfaces,
;           when bit 7 of register C is set, a 23-bit addressing scheme is used
;           and bits 0-6 of register C contain bits 23-16 of the sector number.
_PHYDIO:
	CALL    H.PHYD
    RET

;Address  : #013E
;Function : Reads VDP status register
;Output   : A  - Value which was read
;Registers: A
_RDVDP:
    ; in      a,(099H)
    ;RST 38h
    ;DB 099h
    ;ret
    push ix
    ld ix, 0x0010
    DB 0x5b ; .LIL
    rst 38h
    pop ix
    ret

;Address  : #0047
;Function : Write data in the VDP-register
;Input    : B  - Data to write
;           C  - Number of the register
;Registers: AF, BC
_WRTVDP:
    ld      a,b
    di
    ;out     (099H),a
    RST 28h
    DB 099h
    ld      a,c
    or      080H
    ;out     (099H),a
    RST 28h
    DB 099h
    ei
    push    hl
    ld      a,b
    ld      b,0
    ld      hl,RG0SAV
    add     hl,bc
    ld      (hl),a
    pop     hl
    ret

;    push ix
;    ld ix, 0x0010
;    DB 0x5b ; .LIL
;    rst 38h
;    pop ix
;    ret

;Address  : #0050
;Function : Enable VDP to read
;Input    : HL - For VRAM-address
;Registers: AF
_SETRD:
    ld      a,l
    di
    ;out     (099H),a
    RST 28h
    DB 099h
    ld      a,h
    and     03FH
    ;out     (099H),a
    RST 28h
    DB 099h
    ei
    ret

;Address  : #0053
;Function : Enable VDP to write
;Input    : HL - Address
;Registers: AF
_SETWRT:
    ld      a,l
    di
    ;out     (099H),a
    RST 28h
    DB 099h
    ld      a,h
    and     03FH
    or      040H
    ;out     (099H),a
    RST 28h
    DB 099h
    ei
    ret

;Address  : #004A
;Function : Reads the content of VRAM
;Input    : HL - Address read
;Output   : A  - Value which was read
;Registers: AF
_RDVRM:
    call    SETRD                   ; SETRD
    ex      (sp),hl
    ex      (sp),hl
    ;in      a,(098H)
    RST 38h
    DB 098h
    ret


;Address  : #004D
;Function : Writes data in VRAM
;Input    : HL - Address write
;           A  - Value write
;Registers: AF
_WRTVRM:
    push    af
    call    SETWRT                  ; SETWRT
    ex      (sp),hl
    ex      (sp),hl
    pop     af
    RST 28h
    DB 098h
    ret

;Address  : #0093
;Function : Writes data to PSG register
;Input    : A  - PSG register number
;           E  - Data write
_WRTPSG:
    ret

;Address  : #0096
;Function : Reads value from PSG register
;Input    : A  - PSG register read
;Output   : A  - Value read
_RDPSG:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

RG0SAV_: defb    000H
RG1SAV_: defb    0E0H
RG2SAV_: defb    000H
RG3SAV_: defb    000H
RG4SAV_: defb    000H
RG5SAV_: defb    000H
RG6SAV_: defb    000H
RG7SAV_: defb    000H
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

CGTABL:
        DEFB	000H,000H,000H,000H,000H,000H,000H,000H
        DEFB	03CH,042H,0A5H,081H,0A5H,099H,042H,03CH
        DEFB	03CH,07EH,0DBH,0FFH,0FFH,0DBH,066H,03CH
        DEFB	06CH,0FEH,0FEH,0FEH,07CH,038H,010H,000H
        DEFB	010H,038H,07CH,0FEH,07CH,038H,010H,000H
        DEFB	010H,038H,054H,0FEH,054H,010H,038H,000H
        DEFB	010H,038H,07CH,0FEH,0FEH,010H,038H,000H
        DEFB	000H,000H,000H,030H,030H,000H,000H,000H
        DEFB	0FFH,0FFH,0FFH,0E7H,0E7H,0FFH,0FFH,0FFH
        DEFB	038H,044H,082H,082H,082H,044H,038H,000H
        DEFB	0C7H,0BBH,07DH,07DH,07DH,0BBH,0C7H,0FFH
        DEFB	00FH,003H,005H,079H,088H,088H,088H,070H
        DEFB	038H,044H,044H,044H,038H,010H,07CH,010H
        DEFB	030H,028H,024H,024H,028H,020H,0E0H,0C0H
        DEFB	03CH,024H,03CH,024H,024H,0E4H,0DCH,018H
        DEFB	010H,054H,038H,0EEH,038H,054H,010H,000H
        DEFB	010H,010H,010H,07CH,010H,010H,010H,010H
        DEFB	010H,010H,010H,0FFH,000H,000H,000H,000H
        DEFB	000H,000H,000H,0FFH,010H,010H,010H,010H
        DEFB	010H,010H,010H,0F0H,010H,010H,010H,010H
        DEFB	010H,010H,010H,01FH,010H,010H,010H,010H
        DEFB	010H,010H,010H,0FFH,010H,010H,010H,010H
        DEFB	010H,010H,010H,010H,010H,010H,010H,010H
        DEFB	000H,000H,000H,0FFH,000H,000H,000H,000H
        DEFB	000H,000H,000H,01FH,010H,010H,010H,010H
        DEFB	000H,000H,000H,0F0H,010H,010H,010H,010H
        DEFB	010H,010H,010H,01FH,000H,000H,000H,000H
        DEFB	010H,010H,010H,0F0H,000H,000H,000H,000H
        DEFB	081H,042H,024H,018H,018H,024H,042H,081H
        DEFB	001H,002H,004H,008H,010H,020H,040H,080H
        DEFB	080H,040H,020H,010H,008H,004H,002H,001H
        DEFB	000H,010H,010H,0FFH,010H,010H,000H,000H
        DEFB	000H,000H,000H,000H,000H,000H,000H,000H
        DEFB	020H,020H,020H,020H,000H,000H,020H,000H
        DEFB	050H,050H,050H,000H,000H,000H,000H,000H
        DEFB	050H,050H,0F8H,050H,0F8H,050H,050H,000H
        DEFB	020H,078H,0A0H,070H,028H,0F0H,020H,000H
        DEFB	0C0H,0C8H,010H,020H,040H,098H,018H,000H
        DEFB	040H,0A0H,040H,0A8H,090H,098H,060H,000H
        DEFB	010H,020H,040H,000H,000H,000H,000H,000H
        DEFB	010H,020H,040H,040H,040H,020H,010H,000H
        DEFB	040H,020H,010H,010H,010H,020H,040H,000H
        DEFB	020H,0A8H,070H,020H,070H,0A8H,020H,000H
        DEFB	000H,020H,020H,0F8H,020H,020H,000H,000H
        DEFB	000H,000H,000H,000H,000H,020H,020H,040H
        DEFB	000H,000H,000H,078H,000H,000H,000H,000H
        DEFB	000H,000H,000H,000H,000H,060H,060H,000H
        DEFB	000H,000H,008H,010H,020H,040H,080H,000H
        DEFB	070H,088H,098H,0A8H,0C8H,088H,070H,000H
        DEFB	020H,060H,0A0H,020H,020H,020H,0F8H,000H
        DEFB	070H,088H,008H,010H,060H,080H,0F8H,000H
        DEFB	070H,088H,008H,030H,008H,088H,070H,000H
        DEFB	010H,030H,050H,090H,0F8H,010H,010H,000H
        DEFB	0F8H,080H,0E0H,010H,008H,010H,0E0H,000H
        DEFB	030H,040H,080H,0F0H,088H,088H,070H,000H
        DEFB	0F8H,088H,010H,020H,020H,020H,020H,000H
        DEFB	070H,088H,088H,070H,088H,088H,070H,000H
        DEFB	070H,088H,088H,078H,008H,010H,060H,000H
        DEFB	000H,000H,020H,000H,000H,020H,000H,000H
        DEFB	000H,000H,020H,000H,000H,020H,020H,040H
        DEFB	018H,030H,060H,0C0H,060H,030H,018H,000H
        DEFB	000H,000H,0F8H,000H,0F8H,000H,000H,000H
        DEFB	0C0H,060H,030H,018H,030H,060H,0C0H,000H
        DEFB	070H,088H,008H,010H,020H,000H,020H,000H
        DEFB	070H,088H,008H,068H,0A8H,0A8H,070H,000H
        DEFB	020H,050H,088H,088H,0F8H,088H,088H,000H
        DEFB	0F0H,048H,048H,070H,048H,048H,0F0H,000H
        DEFB	030H,048H,080H,080H,080H,048H,030H,000H
        DEFB	0E0H,050H,048H,048H,048H,050H,0E0H,000H
        DEFB	0F8H,080H,080H,0F0H,080H,080H,0F8H,000H
        DEFB	0F8H,080H,080H,0F0H,080H,080H,080H,000H
        DEFB	070H,088H,080H,0B8H,088H,088H,070H,000H
        DEFB	088H,088H,088H,0F8H,088H,088H,088H,000H
        DEFB	070H,020H,020H,020H,020H,020H,070H,000H
        DEFB	038H,010H,010H,010H,090H,090H,060H,000H
        DEFB	088H,090H,0A0H,0C0H,0A0H,090H,088H,000H
        DEFB	080H,080H,080H,080H,080H,080H,0F8H,000H
        DEFB	088H,0D8H,0A8H,0A8H,088H,088H,088H,000H
        DEFB	088H,0C8H,0C8H,0A8H,098H,098H,088H,000H
        DEFB	070H,088H,088H,088H,088H,088H,070H,000H
        DEFB	0F0H,088H,088H,0F0H,080H,080H,080H,000H
        DEFB	070H,088H,088H,088H,0A8H,090H,068H,000H
        DEFB	0F0H,088H,088H,0F0H,0A0H,090H,088H,000H
        DEFB	070H,088H,080H,070H,008H,088H,070H,000H
        DEFB	0F8H,020H,020H,020H,020H,020H,020H,000H
        DEFB	088H,088H,088H,088H,088H,088H,070H,000H
        DEFB	088H,088H,088H,088H,050H,050H,020H,000H
        DEFB	088H,088H,088H,0A8H,0A8H,0D8H,088H,000H
        DEFB	088H,088H,050H,020H,050H,088H,088H,000H
        DEFB	088H,088H,088H,070H,020H,020H,020H,000H
        DEFB	0F8H,008H,010H,020H,040H,080H,0F8H,000H
        DEFB	070H,040H,040H,040H,040H,040H,070H,000H
        DEFB	000H,000H,080H,040H,020H,010H,008H,000H
        DEFB	070H,010H,010H,010H,010H,010H,070H,000H
        DEFB	020H,050H,088H,000H,000H,000H,000H,000H
        DEFB	000H,000H,000H,000H,000H,000H,0F8H,000H
        DEFB	040H,020H,010H,000H,000H,000H,000H,000H
        DEFB	000H,000H,070H,008H,078H,088H,078H,000H
        DEFB	080H,080H,0B0H,0C8H,088H,0C8H,0B0H,000H
        DEFB	000H,000H,070H,088H,080H,088H,070H,000H
        DEFB	008H,008H,068H,098H,088H,098H,068H,000H
        DEFB	000H,000H,070H,088H,0F8H,080H,070H,000H
        DEFB	010H,028H,020H,0F8H,020H,020H,020H,000H
        DEFB	000H,000H,068H,098H,098H,068H,008H,070H
        DEFB	080H,080H,0F0H,088H,088H,088H,088H,000H
        DEFB	020H,000H,060H,020H,020H,020H,070H,000H
        DEFB	010H,000H,030H,010H,010H,010H,090H,060H
        DEFB	040H,040H,048H,050H,060H,050H,048H,000H
        DEFB	060H,020H,020H,020H,020H,020H,070H,000H
        DEFB	000H,000H,0D0H,0A8H,0A8H,0A8H,0A8H,000H
        DEFB	000H,000H,0B0H,0C8H,088H,088H,088H,000H
        DEFB	000H,000H,070H,088H,088H,088H,070H,000H
        DEFB	000H,000H,0B0H,0C8H,0C8H,0B0H,080H,080H
        DEFB	000H,000H,068H,098H,098H,068H,008H,008H
        DEFB	000H,000H,0B0H,0C8H,080H,080H,080H,000H
        DEFB	000H,000H,078H,080H,0F0H,008H,0F0H,000H
        DEFB	040H,040H,0F0H,040H,040H,048H,030H,000H
        DEFB	000H,000H,090H,090H,090H,090H,068H,000H
        DEFB	000H,000H,088H,088H,088H,050H,020H,000H
        DEFB	000H,000H,088H,0A8H,0A8H,0A8H,050H,000H
        DEFB	000H,000H,088H,050H,020H,050H,088H,000H
        DEFB	000H,000H,088H,088H,098H,068H,008H,070H
        DEFB	000H,000H,0F8H,010H,020H,040H,0F8H,000H
        DEFB	018H,020H,020H,040H,020H,020H,018H,000H
        DEFB	020H,020H,020H,000H,020H,020H,020H,000H
        DEFB	0C0H,020H,020H,010H,020H,020H,0C0H,000H
        DEFB	040H,0A8H,010H,000H,000H,000H,000H,000H
        DEFB	000H,000H,020H,050H,0F8H,000H,000H,000H
        DEFB	070H,088H,080H,080H,088H,070H,020H,060H
        DEFB	090H,000H,000H,090H,090H,090H,068H,000H
        DEFB	010H,020H,070H,088H,0F8H,080H,070H,000H
        DEFB	020H,050H,070H,008H,078H,088H,078H,000H
        DEFB	048H,000H,070H,008H,078H,088H,078H,000H
        DEFB	020H,010H,070H,008H,078H,088H,078H,000H
        DEFB	020H,000H,070H,008H,078H,088H,078H,000H
        DEFB	000H,070H,080H,080H,080H,070H,010H,060H
        DEFB	020H,050H,070H,088H,0F8H,080H,070H,000H
        DEFB	050H,000H,070H,088H,0F8H,080H,070H,000H
        DEFB	020H,010H,070H,088H,0F8H,080H,070H,000H
        DEFB	050H,000H,000H,060H,020H,020H,070H,000H
        DEFB	020H,050H,000H,060H,020H,020H,070H,000H
        DEFB	040H,020H,000H,060H,020H,020H,070H,000H
        DEFB	050H,000H,020H,050H,088H,0F8H,088H,000H
        DEFB	020H,000H,020H,050H,088H,0F8H,088H,000H
        DEFB	010H,020H,0F8H,080H,0F0H,080H,0F8H,000H
        DEFB	000H,000H,06CH,012H,07EH,090H,06EH,000H
        DEFB	03EH,050H,090H,09CH,0F0H,090H,09EH,000H
        DEFB	060H,090H,000H,060H,090H,090H,060H,000H
        DEFB	090H,000H,000H,060H,090H,090H,060H,000H
        DEFB	040H,020H,000H,060H,090H,090H,060H,000H
        DEFB	040H,0A0H,000H,0A0H,0A0H,0A0H,050H,000H
        DEFB	040H,020H,000H,0A0H,0A0H,0A0H,050H,000H
        DEFB	090H,000H,090H,090H,0B0H,050H,010H,0E0H
        DEFB	050H,000H,070H,088H,088H,088H,070H,000H
        DEFB	050H,000H,088H,088H,088H,088H,070H,000H
        DEFB	020H,020H,078H,080H,080H,078H,020H,020H
        DEFB	018H,024H,020H,0F8H,020H,0E2H,05CH,000H
        DEFB	088H,050H,020H,0F8H,020H,0F8H,020H,000H
        DEFB	0C0H,0A0H,0A0H,0C8H,09CH,088H,088H,08CH
        DEFB	018H,020H,020H,0F8H,020H,020H,020H,040H
        DEFB	010H,020H,070H,008H,078H,088H,078H,000H
        DEFB	010H,020H,000H,060H,020H,020H,070H,000H
        DEFB	020H,040H,000H,060H,090H,090H,060H,000H
        DEFB	020H,040H,000H,090H,090H,090H,068H,000H
        DEFB	050H,0A0H,000H,0A0H,0D0H,090H,090H,000H
        DEFB	028H,050H,000H,0C8H,0A8H,098H,088H,000H
        DEFB	000H,070H,008H,078H,088H,078H,000H,0F8H
        DEFB	000H,060H,090H,090H,090H,060H,000H,0F0H
        DEFB	020H,000H,020H,040H,080H,088H,070H,000H
        DEFB	000H,000H,000H,0F8H,080H,080H,000H,000H
        DEFB	000H,000H,000H,0F8H,008H,008H,000H,000H
        DEFB	084H,088H,090H,0A8H,054H,084H,008H,01CH
        DEFB	084H,088H,090H,0A8H,058H,0A8H,03CH,008H
        DEFB	020H,000H,000H,020H,020H,020H,020H,000H
        DEFB	000H,000H,024H,048H,090H,048H,024H,000H
        DEFB	000H,000H,090H,048H,024H,048H,090H,000H
        DEFB	028H,050H,020H,050H,088H,0F8H,088H,000H
        DEFB	028H,050H,070H,008H,078H,088H,078H,000H
        DEFB	028H,050H,000H,070H,020H,020H,070H,000H
        DEFB	028H,050H,000H,020H,020H,020H,070H,000H
        DEFB	028H,050H,000H,070H,088H,088H,070H,000H
        DEFB	050H,0A0H,000H,060H,090H,090H,060H,000H
        DEFB	028H,050H,000H,088H,088H,088H,070H,000H
        DEFB	050H,0A0H,000H,0A0H,0A0H,0A0H,050H,000H
        DEFB	0FCH,048H,048H,048H,0E8H,008H,050H,020H
        DEFB	000H,050H,000H,050H,050H,050H,010H,020H
        DEFB	0C0H,044H,0C8H,054H,0ECH,054H,09EH,004H
        DEFB	010H,0A8H,040H,000H,000H,000H,000H,000H
        DEFB	000H,020H,050H,088H,050H,020H,000H,000H
        DEFB	088H,010H,020H,040H,080H,028H,000H,000H
        DEFB	07CH,0A8H,0A8H,068H,028H,028H,028H,000H
        DEFB	038H,040H,030H,048H,048H,030H,008H,070H
        DEFB	000H,000H,000H,000H,000H,000H,0FFH,0FFH
        DEFB	0F0H,0F0H,0F0H,0F0H,00FH,00FH,00FH,00FH
        DEFB	000H,000H,0FFH,0FFH,0FFH,0FFH,0FFH,0FFH
        DEFB	0FFH,0FFH,000H,000H,000H,000H,000H,000H
        DEFB	000H,000H,000H,03CH,03CH,000H,000H,000H
        DEFB	0FFH,0FFH,0FFH,0FFH,0FFH,0FFH,000H,000H
        DEFB	0C0H,0C0H,0C0H,0C0H,0C0H,0C0H,0C0H,0C0H
        DEFB	00FH,00FH,00FH,00FH,0F0H,0F0H,0F0H,0F0H
        DEFB	0FCH,0FCH,0FCH,0FCH,0FCH,0FCH,0FCH,0FCH
        DEFB	003H,003H,003H,003H,003H,003H,003H,003H
        DEFB	03FH,03FH,03FH,03FH,03FH,03FH,03FH,03FH
        DEFB	011H,022H,044H,088H,011H,022H,044H,088H
        DEFB	088H,044H,022H,011H,088H,044H,022H,011H
        DEFB	0FEH,07CH,038H,010H,000H,000H,000H,000H
        DEFB	000H,000H,000H,000H,010H,038H,07CH,0FEH
        DEFB	080H,0C0H,0E0H,0F0H,0E0H,0C0H,080H,000H
        DEFB	001H,003H,007H,00FH,007H,003H,001H,000H
        DEFB	0FFH,07EH,03CH,018H,018H,03CH,07EH,0FFH
        DEFB	081H,0C3H,0E7H,0FFH,0FFH,0E7H,0C3H,081H
        DEFB	0F0H,0F0H,0F0H,0F0H,000H,000H,000H,000H
        DEFB	000H,000H,000H,000H,00FH,00FH,00FH,00FH
        DEFB	00FH,00FH,00FH,00FH,000H,000H,000H,000H
        DEFB	000H,000H,000H,000H,0F0H,0F0H,0F0H,0F0H
        DEFB	033H,033H,0CCH,0CCH,033H,033H,0CCH,0CCH
        DEFB	000H,020H,020H,050H,050H,088H,0F8H,000H
        DEFB	020H,020H,070H,020H,070H,020H,020H,000H
        DEFB	000H,000H,000H,050H,088H,0A8H,050H,000H
        DEFB	0FFH,0FFH,0FFH,0FFH,0FFH,0FFH,0FFH,0FFH
        DEFB	000H,000H,000H,000H,0FFH,0FFH,0FFH,0FFH
        DEFB	0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,0F0H,0F0H
        DEFB	00FH,00FH,00FH,00FH,00FH,00FH,00FH,00FH
        DEFB	0FFH,0FFH,0FFH,0FFH,000H,000H,000H,000H
        DEFB	000H,000H,068H,090H,090H,090H,068H,000H
        DEFB	030H,048H,048H,070H,048H,048H,070H,0C0H
        DEFB	0F8H,088H,080H,080H,080H,080H,080H,000H
        DEFB	0F8H,050H,050H,050H,050H,050H,098H,000H
        DEFB	0F8H,088H,040H,020H,040H,088H,0F8H,000H
        DEFB	000H,000H,078H,090H,090H,090H,060H,000H
        DEFB	000H,050H,050H,050H,050H,068H,080H,080H
        DEFB	000H,050H,0A0H,020H,020H,020H,020H,000H
        DEFB	0F8H,020H,070H,0A8H,0A8H,070H,020H,0F8H
        DEFB	020H,050H,088H,0F8H,088H,050H,020H,000H
        DEFB	070H,088H,088H,088H,050H,050H,0D8H,000H
        DEFB	030H,040H,040H,020H,050H,050H,050H,020H
        DEFB	000H,000H,000H,050H,0A8H,0A8H,050H,000H
        DEFB	008H,070H,0A8H,0A8H,0A8H,070H,080H,000H
        DEFB	038H,040H,080H,0F8H,080H,040H,038H,000H
        DEFB	070H,088H,088H,088H,088H,088H,088H,000H
        DEFB	000H,0F8H,000H,0F8H,000H,0F8H,000H,000H
        DEFB	020H,020H,0F8H,020H,020H,000H,0F8H,000H
        DEFB	0C0H,030H,008H,030H,0C0H,000H,0F8H,000H
        DEFB	018H,060H,080H,060H,018H,000H,0F8H,000H
        DEFB	010H,028H,020H,020H,020H,020H,020H,020H
        DEFB	020H,020H,020H,020H,020H,020H,0A0H,040H
        DEFB	000H,020H,000H,0F8H,000H,020H,000H,000H
        DEFB	000H,050H,0A0H,000H,050H,0A0H,000H,000H
        DEFB	000H,018H,024H,024H,018H,000H,000H,000H
        DEFB	000H,030H,078H,078H,030H,000H,000H,000H
        DEFB	000H,000H,000H,000H,030H,000H,000H,000H
        DEFB	03EH,020H,020H,020H,0A0H,060H,020H,000H
        DEFB	0A0H,050H,050H,050H,000H,000H,000H,000H
        DEFB	040H,0A0H,020H,040H,0E0H,000H,000H,000H
        DEFB	000H,038H,038H,038H,038H,038H,038H,000H
        DEFB	000H,000H,000H,000H,000H,000H,000H,000H

        .DEPHASE
ENDADR:
