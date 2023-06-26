MEM	EQU	62		;for a 62k system (TS802 TEST - WORKS OK).
CBASE EQU (MEM-7)*1024

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC

    ORG 0F200h
BEGIN:
START_BASIC:
;
;**************************************************************
;*
;*        B I O S   J U M P   T A B L E
;*
;**************************************************************
;
BOOT:	JP	_BOOT
WBOOT:	JP	_WBOOT
CONST:	JP	_CONST
CONIN:	JP	_CONIN
CONOUT:	JP  _CONOUT
LIST:	JP  _LIST
PUNCH:	JP  _PUNCH
READER:	JP  _READER
HOME:	JP  _HOME
SELDSK:	JP	_SELDSK
SETTRK:	JP  _SETTRK
SETSEC:	JP  _SETSEC
SETDMA:	JP  _SETDMA
READ:	JP  _READ
WRITE:	JP  _WRITE
PRSTAT:	JP  _PRSTAT
SECTRN:	JP  _SECTRN
;
;*
;******************   E N D   O F   C P / M   *****************
;*

; IMPLEMENTATION FOR ELECTRON OS by S0urceror
_BOOT:
	ld hl, SIGNON
_BOOT_MSG_NEXT
	ld a, (hl)
	and a
	jr z, _WBOOT
	inc hl
	DB 0x5b ; .LIL
	RST 08h
	jr _BOOT_MSG_NEXT
	;
_WBOOT:
    ; load CCP+BDOS
	ld ix, 00002h ; eos_machine_reload_warmboot_images
    DB 0x5b ; .LIL
    RST 38h
    ; set base variables
	ld hl,0
	ld (_TRK),hl
	ld (_SEC),hl
	xor a
	ld (0x0003),a ; IOBYTE
    ; replace BOOT with WBOOT
	ld hl, WBOOT 
	ld (0x0001),hl
    ; Start CCP - CP/M command processor
    ld c, 0 ; default user 0 / drive 0
    jp CBASE
_CONST:
    DB 0x5b ; .LIL
    RST 20h
	and a
    ret z
    ld a, 0ffh
    RET
_CONIN:
    DB 0x5b ; .LIL
    RST 10h
    jr c, _CONIN
    RET
_CONOUT:
	push af
	ld a, c
	DB 0x5b ; .LIL
	RST 08h
	pop af
	RET
_HOME:
	push af
	xor a
	LD (_TRK),A
	pop af
	RET
	
; combined read/write modeled after phydio msx bios call
_READ:
	AND A ; clear the carry flag
	JP _PHYDIO
_WRITE:
    SCF ; set the carry flag
_PHYDIO:
	push af ; store carry flag
    call calc_logical_sector_nr ; result in DE = (_TRK)*(dpb)+(_SEC)
	pop af  ; restore carry flag
	ld hl, (_DMA)
    ld c, 0 ; 0xf9 is double-sided, 80 track, 9 sector, 720k
    ld b, 1 ; 1 sector
	ld a, 0 ; drive A:, set to 0 without destroying carry
	ld ix,00000h ; eos_machine_read_write_disk
    DB 0x5b ; .LIL
    RST 38h ; returns error code in A or 0 when okay
    RET

_LIST:
	RET
_PUNCH:
	RET
_READER:
	LD A, 26 ; return CTRL-Z
	RET
_SETTRK:
	LD (_TRK),BC
    RET
_SETSEC:
	LD (_SEC),BC
    RET
_SETDMA:
	LD (_DMA),BC
    RET
_PRSTAT:
	XOR A
	RET
_SECTRN:
	LD HL,BC
    RET
;
; Multiply 16-bit values (with 16-bit result)
; In: Multiply BC with DE
; Out: HL = result
;
Mult16:
    ld a,b
    ld b,16
Mult16_Loop:
    add hl,hl
    sla c
    rla
    jr nc,Mult16_NoAdd
    add hl,de
Mult16_NoAdd:
    djnz Mult16_Loop
    ret

calc_logical_sector_nr:
	ld hl, 0
	ld bc, (_TRK)
	ld de, (dpb)
	call Mult16
	ld bc, (_SEC)
	add hl, bc
	ld de, hl
	ret

_SELDSK:
	push af
	and a
	jr nz, _seldsk_nodrive
    LD HL,drive0
	pop af
    RET
_seldsk_nodrive:
	ld hl, 0
	pop af
	ret

SIGNON:
	DB "\r\n62k - CP/M 2.2\r\n",0
; variables
;
_TRK: DW 0
_SEC: DW 0
_DMA: DW 0


drive0:
	DW	0	;Address of sector translation table
	DW	0,0,0	;Used as workspace by CP/M
	DW	dirbuf	;Address of a 128-byte sector buffer; this is 
			;the same for all DPHs in the system.
	DW	dpb	;Address of the DPB 
			;giving the format of this drive.
	DW	csv	;Address of the directory checksum vector
			;for this drive.
	DW	alv	;Address of the allocation vector
			;for this drive.

; every disk the same
dirbuf:
	DS 128

dpb:
	DEFW	1024	;Number of 128-byte records per track
	DEFB	5	;Block shift. 3 => 1k, 4 => 2k, 5 => 4k....
	DEFB	31	;Block mask. 7 => 1k, 0Fh => 2k, 1Fh => 4k...
	DEFB	1	;Extent mask, see later
	DEFW	2048-1	;(no. of blocks on the disc)-1
	DEFW	512-1	;(no. of directory entries)-1
	DEFB	0xf0	;Directory allocation bitmap, first byte
	DEFB	0x00	;Directory allocation bitmap, second byte
	DEFW	512/4	;Checksum vector size, 0 for a fixed disc
			;No. directory entries/4, rounded up.
	DEFW	1	;Offset, number of reserved tracks

; per disk a separate CSV and ALV
csv:
	DS 128
alv:
	DS 256

ENDADR: