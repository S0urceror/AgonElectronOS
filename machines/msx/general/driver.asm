; A template driver section
; (C) 1992-2005 by Ultrasoft
;
; By:	Arjen Zeilemaker
;
; Version:		0.90




; symbols which can be used from the kernel

; GETSLT	get my slotid
; DIV16		divide
; GETWRK	get my workarea
; SETINT	install my interrupt handler
; PRVINT	call orginal interrupt handler
; PROMPT	prompt for phantom drive
; RAWFLG	verify flag
; $SECBUF	temporary sectorbuffer
; XFER		transfer to TPA
; DISINT	inform interrupts are being disabled
; ENAINT	inform interrupts are being enabled
; PROCNM	CALL statement name

; symbols which must be defined by the driver

; INIHRD	initialize diskdriver hardware
; DRIVES	how many drives are connected
; INIENV	initialize diskdriver workarea
; DSKIO		diskdriver sector i/o
; DSKCHG	diskdriver diskchange status
; GETDPB	build Drive Parameter Block
; CHOICE	get format choice string
; DSKFMT	format disk
; MTOFF		stop diskmotor
; OEMSTA	diskdriver special call statements

; MYSIZE	size of diskdriver workarea
; SECLEN	size of biggest sector supported by the diskdriver
; DEFDPB	pointer to a default Drive Parameter Block


; errorcodes used by DSKIO, DSKCHG and GETDPB
;
; 0	write protect error
; 2	not ready error
; 4	data (crc) error
; 6	seek error
; 8	record not found error
; 10	write fault error
; 12	other error
;
; DOS2 only errorcodes
;
; 14    ?
; 16    ?
; 20    not a DOS disk
; 22    incompatible disk
; 24    disk not formated
; 26    disk changed

; errorcodes used by DSKFMT
;
; 0	write protect error
; 2	not ready error
; 4	data (crc) error
; 6	seek error
; 8	record not found error
; 10	write fault error
; 12	bad parameter
; 14	insufficient memory
; 16	other error



MYSIZE		equ	1		; Size of environment
SECLEN		equ	512		; Size of biggest sector

; INIHRD
;
; Input:	None
; Output:	None
; Changed:	AF,BC,DE,HL,IX,IY may be affected

INIHRD:
    ret

; DRIVES
;
; Input: 	F	Zx set if to return physical drives
;			Zx reset if to return at least 2 drives, if only one
;			  physical drive it becomes a phantom drive
; Output:	L	number of drives
; Changed:	F,HL,IX,IY may be affected
;
; Remark:	DOS1 does not handle L=0 correctly

DRIVES:
    ld	l,1
    ret

; INIENV
;
; Input: 	None
; Output:	None
; Changed:	AF,BC,DE,HL,IX,IY may be affected
INIENV:
    RET
;
; DSKIO
;
; Input: 	A	Drivenumber
;		F	Cx reset for read
;			Cx set for write
; 		B	number of sectors
; 		C	Media descriptor
;		DE	logical sectornumber
; 		HL	transferaddress
; Output:	F	Cx set for error
;			Cx reset for ok
;		A	if error, errorcode
;		B	if error, remaining sectors
; Changed:	AF,BC,DE,HL,IX,IY may be affected

DSKIO:
    ld ix,00000h ; eos_machine_read_write_disk
    DB 0x5b ; .LIL
    RST 38h ; returns error code in A or 0 when okay
    ret

; DSKCHG
;
; Input: 	A	Drivenumber
; 		B	0
; 		C	Media descriptor
; 		HL	pointer to DPB
; Output:	F	Cx set for error
;			Cx reset for ok
;		A	if error, errorcode
;		B	if no error, disk change status
;			01 disk unchanged
;			00 unknown
;			FF disk changed
; Changed:	AF,BC,DE,HL,IX,IY may be affected
; Remark:	DOS1 kernel expects the DPB updated when disk change status is unknown or changed
;		DOS2 kernel does not care if the DPB is updated or not

DSKCHG:
    or	a
    ld	b,1
    ret

; GETDPB
;
; Input: 	A	Drivenumber
; 		B	first byte of FAT
; 		C	Media descriptor
; 		HL	pointer to DPB
; Output:	[HL+1]
;		..
;		[HL+18]	updated
; Changed:	AF,BC,DE,HL,IX,IY may be affected

GETDPB:
    EX	DE,HL
    INC	DE
    LD	A,B
    SUB	0F8H			; mediadescriptor 0F8H-0FFH ?
    RET	C			; nope, quit with error
    CP	4			; mediadescriptor 0F8H-0FBH ?
    JR	NC,DPBERROR		; nope, quit with WRITE FAULT error
    RLCA
    LD	C,A
    LD	B,0
    LD	L,A
    LD	H,B
    ADD	HL,HL
    ADD	HL,HL
    ADD	HL,HL
    ADD	HL,BC
    LD	BC,DPBTABLE
    ADD	HL,BC
    LD	BC,18
    LDIR
    RET
DPBERROR:	
    LD	A,10
    SCF
    RET


; CHOICE
;
; Input: 	None
; Output:	HL	pointer to choice string, 0 if no choice
; Changed:	AF,BC,DE,HL,IX,IY may be affected

CHOICE:
    ld	hl,ChoiceStr
    ret

ChoiceStr:	db	13,10
                db	"1 - Choice A",13,10
                db	"2 - Choice B",13,10
                db	13,10
                db	0

; DSKFMT
;
; Input: 	A	choicecode (1-9)
;		D	drivenumber
;		HL	begin of workarea
;		BC	length of workarea
; Output:	F	Cx set for error
;			Cx reset for ok
;		A	if error, errorcode
; Changed:	AF,BC,DE,HL,IX,IY may be affected

DSKFMT:
    ld	a,16
    scf
    ret

; OEMSTATEMENT
;
; Input:	HL	basicpointer
; Output:	F	Cx set if statement not recognized
;			Cx reset if statement is recognized
;		HL	basicpointer,	updated if recognized
;					unchanged if not recognized
; Changed:	AF,BC,DE,HL,IX,IY may be affected

OEMSTA:
    scf
    ret

; MTOFF
;
; Input:	None
; Output:	None
; Changed:	AF,BC,DE,HL,IX,IY may be affected

MTOFF:
    ret


DPBTABLE:	
        DEFB	0F8h		; Media F8
        DEFW	512		; 80 Tracks
        DEFB	0Fh		; 9 sectors
        DEFB	04h		; 1 side
        DEFB	01h		; 3.5" 360 Kb
        DEFB	02h
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	12
        DEFW	355
        DEFB	2
        DEFW	5

DEFDPB  EQU     $-1

        DEFB	0F9h		; Media F9
        DEFW	512		; 80 Tracks
        DEFB	0Fh		; 9 sectors
        DEFB	04h		; 2 sides
        DEFB	01h		; 3.5" 720 Kb
        DEFB	02h
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	14
        DEFW	714
        DEFB	3
        DEFW	7

        DEFB	0FAh		; Media FA
        DEFW	512		; 80 Tracks
        DEFB	0Fh		; 8 sectors
        DEFB	04h		; 1 side
        DEFB	01h		; 3.5" 320 Kb
        DEFB	02h
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	10
        DEFW	316
        DEFB	1
        DEFW	3

        DEFB	0FBh		; Media FB
        DEFW	512		; 80 Tracks
        DEFB	0Fh		; 8 sectors
        DEFB	04h		; 2 sides
        DEFB	01h		; 3.5" 640 Kb
        DEFB	02h
        DEFW	1
        DEFB	2
        DEFB	112
        DEFW	12
        DEFW	635
        DEFB	2
        DEFW	5