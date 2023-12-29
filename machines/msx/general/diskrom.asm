    DEVICE NOSLOT64K
    PAGE 2
    
; DOS1.MAC

; DOS 1.03 kernel (latest version known, SONY HB-F1XV)
; version number is not offical, it is created by me
; Assume this is version Aug 23, 1985 (fixed control-C problem)

; Source re-created by Z80DIS 2.2
; Z80DIS was written by Kenneth Gielow, Palo Alto, CA

; Code Copyrighted by ASCII and maybe others
; Source comments by Arjen Zeilemaker

; Sourcecode supplied for STUDY ONLY
; Recreation NOT permitted without authorisation of the copyrightholders


        IFDEF AGONELECTRONBIN
                ; BLOAD header
                db 0x0fe
                dw BEGIN, ENDADR, 0
        ENDIF

        ORG     04000H
BEGIN:

; symbols which must be defined by the disk hardware driver

;       EXTRN   INIHRD
;       EXTRN   DRIVES
;       EXTRN   INIENV
;       EXTRN   DSKIO
;       EXTRN   DSKCHG
;       EXTRN   GETDPB
;       EXTRN   CHOICE
;       EXTRN   DSKFMT
;       EXTRN   MTOFF
;       EXTRN   OEMSTA
;       EXTRN   MYSIZE
;       EXTRN   SECLEN
;       EXTRN   DEFDPB

; symbols of routines which can be used by the disk hardware driver

;       PUBLIC  PROMPT
;       PUBLIC  SETINT
;       PUBLIC  PRVINT
;       PUBLIC  GETSLT
;       PUBLIC  GETWRK
;       PUBLIC  DIV16
;       PUBLIC  ENASLT
;       PUBLIC  XFER


WBOOT   equ     00000H
RDSLT   equ     0000CH
WRSLT   equ     00014H
CALSLT  equ     0001CH
ENASLT  equ     00024H
IDBYT0  equ     0002BH
CALLF   equ     00030H
M0034   equ     00034H
KEYINT  equ     00038H
LDIRMV  equ     00059H
LDIRVM  equ     0005CH
CHSNS   equ     0009CH
CHGET   equ     0009FH
CHPUT   equ     000A2H
LPTOUT  equ     000A5H
BREAKX  equ     000B7H
CKCNTC  equ     000BDH
ERAFNK  equ     000CCH
TOTEXT  equ     000D2H
SNSMAT  equ     00141H
PHYDIO  equ     00144H
KILBUF  equ     00156H
CALBAS  equ     00159H

X003B   equ     0003BH                  ; helper routine in DOS memoryspace: store and change secundairy slotregister
X0046   equ     00046H                  ; helper routine in DOS memoryspace: restore secundairy slotregister (RDSLT/WRSLT)
X004B   equ     0004BH                  ; helper routine in DOS memoryspace: restore secundairy slotregister (CALSLT/CALLF)

XF1C9   equ     0F1C9H                  ; output string (BDOS 9), also start of fixed diskvars
XF1D9   equ     0F1D9H                  ; XFER, transfer to DOS memory
XF1E2   equ     0F1E2H                  ; WBOOT, warm boot DOS system
XF1E8   equ     0F1E8H                  ; start handler in DOS memory
XF1F4   equ     0F1F4H                  ; validate FCB filename
YF1F7   equ     0F1F7H                  ; reserved devicenames
YF20B   equ     0F20BH                  ; direntry for devices
YF22B   equ     0F22BH                  ; days in month table
YF237   equ     0F237H                  ; BDOS console output columnpos
YF238   equ     0F238H                  ; console columnpos at start of lineinput
YF239   equ     0F239H                  ; lineinput insert flag
YF23A   equ     0F23AH                  ; lineinput secret message flag
YF23B   equ     0F23BH                  ; console output to printer flag
YF23C   equ     0F23CH                  ; directory buffer changed flag
YF23D   equ     0F23DH                  ; transferaddress
YF23F   equ     0F23FH                  ; sectornumber in data buffer
YF241   equ     0F241H                  ; driveid of sector in data buffer
YF242   equ     0F242H                  ; data buffer changed flag
YF243   equ     0F243H                  ; DPB pointer current operation
YF245   equ     0F245H                  ; sectornumber (offset) in directory buffer
YF246   equ     0F246H                  ; driveid of sector in directory buffer
YF247   equ     0F247H                  ; default driveid
YF248   equ     0F248H                  ; current day (1..31)
YF249   equ     0F249H                  ; current month (1..12)
YF24A   equ     0F24AH                  ; current year (offset to 1980)
YF24C   equ     0F24CH                  ; current days since 1-1-1980
YF24E   equ     0F24EH                  ; current day of week (0=sunday)

XF24F   equ     0F24FH                  ; prompt for disk hook
XF252   equ     0F252H                  ; get fat entry content hook
XF255   equ     0F255H                  ; check if devicename hook
XF258   equ     0F258H                  ; try next direntry hook
XF25B   equ     0F25BH                  ; get next direntry hook
XF25E   equ     0F25EH                  ; next direntry hook
XF261   equ     0F261H                  ; validate FCB drive and filename hook
XF264   equ     0F264H                  ; fcb open hook
XF267   equ     0F267H                  ; get latest FAT hook
XF26A   equ     0F26AH                  ; get pointer to DPB of current drive hook
XF26D   equ     0F26DH                  ; write FAT hook
XF270   equ     0F270H                  ; read sector hook
XF273   equ     0F273H                  ; diskerror hook
XF276   equ     0F276H                  ; write dirsector hook
XF279   equ     0F279H                  ; write sector hook
XF27C   equ     0F27CH                  ; multiply hook
XF27F   equ     0F27FH                  ; divide hook
XF282   equ     0F282H                  ; get absolute cluster hook
XF285   equ     0F285H                  ; get next absolute cluster hook
XF288   equ     0F288H                  ; partical sector read hook
XF28B   equ     0F28BH                  ; partical sector write hook
XF28E   equ     0F28EH                  ; start read recordoperation from disk hook
XF291   equ     0F291H                  ; finish read recordoperation from disk hook
XF294   equ     0F294H                  ; end read recordoperation from disk hook
XF297   equ     0F297H                  ; record operation error at start hook
XF29A   equ     0F29AH                  ; start write recordoperation to disk hook
XF29D   equ     0F29DH                  ; finish write recordoperation to disk hook
XF2A0   equ     0F2A0H                  ; calculate sequencial sectors hook
XF2A3   equ     0F2A3H                  ; get sectornumber of cluster hook
XF2A6   equ     0F2A6H                  ; allocate FAT chain hook
XF2A9   equ     0F2A9H                  ; release FAT chain hook
XF2AC   equ     0F2ACH                  ; lineinput headloop hook
XF2AF   equ     0F2AFH                  ; console output hook
XF2B2   equ     0F2B2H                  ; get time and date for direntry hook
XF2B5   equ     0F2B5H                  ; setup days in februari hook

YF2B8   equ     0F2B8H                  ; current direntry number
YF2B9   equ     0F2B9H                  ; filename1
YF2C4   equ     0F2C4H                  ; original DR byte FCB
YF2C5   equ     0F2C5H                  ; filename2 (rename)
YF2D0   equ     0F2D0H                  ; temporary save for F2B9 and F2C4 (rename)
YF2DC   equ     0F2DCH                  ; flag ignore fileattributes
YF2DD   equ     0F2DDH                  ; current relative sector in cluster
YF2DE   equ     0F2DEH                  ; result of recordoperation
YF2DF   equ     0F2DFH                  ; flag increase current relative sector in cluster (0 means not)
YF2E0   equ     0F2E0H                  ; flag flake read (0 means real read)
YF2E1   equ     0F2E1H                  ; current driveid
YF2E2   equ     0F2E2H                  ; transferaddress for recordoperation
YF2E4   equ     0F2E4H                  ; start record (32 bit) for recordoperation
YF2E8   equ     0F2E8H                  ; number of records for recordoperation
YF2EA   equ     0F2EAH                  ; current relative cluster of file
YF2EC   equ     0F2ECH                  ; current cluster of file
YF2EE   equ     0F2EEH                  ; start relative sector for recordoperation
YF2F0   equ     0F2F0H                  ; relative cluster after fileend for write recordoperation
YF2F2   equ     0F2F2H                  ; start offset in sector for recordoperation
YF2F4   equ     0F2F4H                  ; start fileposition (32 bit) for recordoperation
YF2F8   equ     0F2F8H                  ; partical sector transfer at start
YF2FA   equ     0F2FAH                  ; partical sector transfer at end
YF2FC   equ     0F2FCH                  ; number of complete sectors to transfer
YF2FE   equ     0F2FEH                  ; first free direntry (0FFH if none found)
YF2FF   equ     0F2FFH                  ; flag diskoperation (0 if read, 1 if write)
YF300   equ     0F300H                  ; pointer to remaining lineinput from CON read record operation
YF302   equ     0F302H                  ; temporary store for maximium cluster
                                        ; looks like F304-F305 are unused!
YF306   equ     0F306H                  ; flag CP/M compatible BDOS call (0 means no CP/M, HL has value, <>0 means CP/M, HL is compatible filled)
YF307   equ     0F307H                  ; saved pointer to FCB search first, used for search next
YF309   equ     0F309H                  ; saved pointer to DPB search first, used for search next
YF30B   equ     0F30BH                  ; saved current direntry number search first/next (FFh means invalid)
YF30C   equ     0F30CH                  ; original EX byte FCB
RAWFLG  equ     0F30DH                  ; read after write (verify) flag
YF30E   equ     0F30EH                  ; date format (0 japanese, 1 european, 2 american)
KANJTA  equ     0F30FH
YF30F   equ     0F30FH                  ; double byte header char table
FUTURE  equ     0F313H                  ; looks like F313-F322 are unused
DISKVE  equ     0F323H
YF323   equ     0F323H                  ; diskerror handler pointer
BREAKV  equ     0F325H
YF325   equ     0F325H                  ; abort handler pointer
AUXBOD  equ     0F327H
XF327   equ     0F327H                  ; AUX input hook (MSXHOOK style), default returns CTRL-Z in register A
XF32C   equ     0F32CH                  ; AUX output hook (MSXHOOK style), default does nothing
BDOSBO  equ     0F331H
XF331   equ     0F331H                  ; BDOS hook (MSXHOOK style)
YF336   equ     0F336H                  ; flag saved input available (0 = none available)
YF337   equ     0F337H                  ; save input
TIMFLG  equ     0F338H
YF338   equ     0F338H                  ; use clockchip flag
YF339   equ     0F339H                  ; saved stackpointer format routine
YF33B   equ     0F33BH                  ; days since 1-1-1980, used when no clockchip
YF33D   equ     0F33DH                  ; recordsize GET/PUT recordoperations
YF33F   equ     0F33FH                  ; at systeminit: CTRL key flag, later: saved driveid driveroperation (0=A:)
YF340   equ     0F340H                  ; flag kernel cold boot (0 = cold, <>0 = warm)
RAMAD0  equ     0F341H                  ; slotid DOS ram page 0
RAMAD1  equ     0F342H                  ; slotid DOS ram page 1
RAMAD2  equ     0F343H                  ; slotid DOS ram page 2
RAMAD3  equ     0F344H                  ; slotid DOS ram page 3
YF345   equ     0F345H                  ; maximum number of diskbasic FCB's
YF346   equ     0F346H                  ; flag MSXDOS has been running (0 = no MSXDOS yet)
YF347   equ     0F347H                  ; number of drives in disksystem
MASTER  equ     0F348H
YF348   equ     0F348H                  ; slotid disksytem rom
HIMSAV  equ     0F349H
YF349   equ     0F349H                  ; disksystem bottom (lowest address used by the disksystem)
DOSHIM  equ     0F34BH
YF34B   equ     0F34BH                  ; msxdos system bottom
SECBUF equ     0F34DH                  ; pointer to sectorbuffer, can be used by the disk hardware driver
YF34F   equ     0F34FH                  ; pointer to datasectorbuffer
YF351   equ     0F351H                  ; pointer to directorysectorbuffer
YF353   equ     0F353H                  ; pointer to the diskbasic FCB's
YF355   equ     0F355H                  ; DPB table for 8 drives
RSLREG  equ     0F365H
XF365   equ     0F365H                  ; routine read primary slotregister
DOSON  equ     0F368H
XF368   equ     0F368H                  ; routine enable disksystem rom on page 1
DOSOF  equ     0F36BH
XF36B   equ     0F36BH                  ; routine enable dos ram on page 1
XFER    equ     0F36EH                  ; routine transfer to/from dos ram
AUXIN  equ     0F371H
XF371   equ     0F371H                  ; auxiliary input routine
AUXOU  equ     0F374H
XF374   equ     0F374H                  ; auxiliary output routine
BLDCHK  equ     0F377H
XF377   equ     0F377H                  ; routine diskbasic BLOAD
XF37A   equ     0F37AH                  ; routine diskbasic BSAVE
BSVCHK  equ     0F37AH
BDOS    equ     0F37DH
XF37D   equ     0F37DH                  ; BDOS entry point

RDPRIM  equ     0F380H
WRPRIM  equ     0F385H
CLPRIM  equ     0F38CH
CLPRM1  equ     0F398H
LINLEN  equ     0F3B0H
CNSDFG  equ     0F3DEH
LPTPOS  equ     0F415H
PRTFLG  equ     0F416H
CURLIN  equ     0F41CH
KBUF    equ     0F41FH
BUF     equ     0F55EH
TTYPOS  equ     0F661H
VALTYP  equ     0F663H
MEMSIZ  equ     0F672H
STKTOP  equ     0F674H
TXTTAB  equ     0F676H
TEMPPT  equ     0F678H
TEMPST  equ     0F67AH
DSCTMP  equ     0F698H
FRETOP  equ     0F69BH
AUTLIN  equ     0F6ABH
SAVSTK  equ     0F6B1H
VARTAB  equ     0F6C2H
STREND  equ     0F6C6H
DAC     equ     0F7F6H
ARG     equ     0F847H
MAXFIL  equ     0F85FH
FILTAB  equ     0F860H
NULBUF  equ     0F862H
PTRFIL  equ     0F864H
FILNAM  equ     0F866H
NLONLY  equ     0F87CH
SAVEND  equ     0F87DH
HOKVLD  equ     0FB20H
YFB21   equ     0FB21H                  ; disk interface table
YFB29   equ     0FB29H                  ; disk interface interrupt table
BOTTOM  equ     0FC48H
HIMEM   equ     0FC4AH
FLBMEM  equ     0FCAEH
RUNBNF  equ     0FCBEH
SAVENT  equ     0FCBFH
EXPTBL  equ     0FCC1H
SLTTBL  equ     0FCC5H
SLTATR  equ     0FCC9H
SLTWRK  equ     0FD09H
PROCNM  equ     0FD89H
DEVICE  equ     0FD99H			; used temp for disk interface count

H.TIMI  equ     0FD9FH
H.DSKO  equ     0FDEFH
H.NAME  equ     0FDF9H
H.KILL  equ     0FDFEH
H.COPY  equ     0FE08H
H.DSKF  equ     0FE12H
H.DSKI  equ     0FE17H
H.LSET  equ     0FE21H
H.RSET  equ     0FE26H
H.FIEL  equ     0FE2BH
H.MKI  equ     0FE30H
H.MKS  equ     0FE35H
H.MKD  equ     0FE3AH
H.CVI   equ     0FE3FH
H.CVS   equ     0FE44H
H.CVD   equ     0FE49H
H.GETP  equ     0FE4EH
H.NOFO  equ     0FE58H
H.NULO  equ     0FE5DH
H.NTFL  equ     0FE62H
H.BINS  equ     0FE71H
H.BINL  equ     0FE76H
H.FILE  equ     0FE7BH
H.DGET  equ     0FE80H
H.FILO  equ     0FE85H
H.INDS  equ     0FE8AH
H.LOC   equ     0FE99H
H.LOF   equ     0FE9EH
H.EOF   equ     0FEA3H
H.BAKU  equ     0FEADH
H.PARD  equ     0FEB2H
H.NODE  equ     0FEB7H
H.POSD  equ     0FEBCH
H.RUNC  equ     0FECBH
H.CLEA  equ     0FED0H
H.LOPD  equ     0FED5H
H.STKE  equ     0FEDAH
H.ERRP  equ     0FEFDH
H.PHYD  equ     0FFA7H
H.FORM  equ     0FFACH
EXTBIO  equ     0FFCAH
DISINT  equ     0FFCFH
ENAINT  equ     0FFD4H

YFFFF   equ     0FFFFH


YC000   equ     0C000H          ; bootsector transferaddress
YCONBF  equ     KBUF+186        ; KBUF is reused for read CON device records, size 127+2
YCONTP  equ     KBUF+58         ; KBUF is reused for temporary buffer buffered input, size 128


; Basic routines

DECSUB  equ     0268CH          ; dbl subtract
DECADD  equ     0269AH          ; dbl add
DECDIV  equ     0289FH          ; dbl divide
VMOVE   equ     02EF3H          ; copy variable content
VMOVFM  equ     02F08H          ; copy variable content to DAC
VMOVMF  equ     02F10H          ; copy variable content from DAC
MAKINT  equ     02F99H          ; integer to DAC
CONDS   equ     03042H          ; convert DAC from sgn to dbl
INT     equ     030D1H          ; dbl to integer
SGNMUL  equ     0325CH          ; sgn multiply
NULSTR  equ     03FD6H          ; empty string
ERROR   equ     0406FH          ; BASIC error
READYR  equ     0409BH          ; restart BASIC
M4173   equ     04173H          ; execute statement
LINKER  equ     04253H          ; recalculate linepointers
NEWSTT  equ     04601H          ; execution loop
CHRGTR  equ     04666H          ; CHRGTR
FLTLIN  equ     046FFH          ; convert to SNG
INTID2  equ     04756H          ; evaluate word operand and check for 0-32767 range
FINPRT  equ     04AFFH          ; output back to screen
FRMEQL  equ     04C5FH          ; evaluate =expression
FRMEVL  equ     04C64H          ; evaluate expression
DOCNVF  equ     0517AH          ; convert DAC to other type
GTBYTC  equ     0521BH          ; evaluate next byte operand
GETBYT  equ     0521CH          ; evaluate byte operand
CONINT  equ     0521FH          ; convert to byte
GETUIN  equ     0542FH          ; evaluate address operand
M5432   equ     05432H          ; convert address to integer
SCCPTR  equ     054F7H          ; convert pointers to linenumbers
GETYPR  equ     05597H          ; GETYPR
PTRGET  equ     05EA4H          ; get address of variable
STRINI  equ     06627H          ; allocate temp string
GETSPA  equ     0668EH          ; allocate stringspace
FRESTR  equ     067D0H          ; free temporary string
FILEVL  equ     06A0EH          ; evaluate filespecification
FILIDX  equ     06A6DH          ; get i/o channel pointer
OPNFIL  equ     06AFAH          ; open i/o channel
CLSFIL  equ     06B24H          ; close i/o channel
CLSALL  equ     06C1CH          ; close all i/o channels
NOSKCR  equ     06E41H          ; resume character putback routine
BSAVE   equ     06E92H          ; start of BSAVE routine
BLOAD   equ     06EC6H          ; start of BLOAD routine
BLDFIN  equ     06EF4H          ; finish BLOAD
M6F0B   equ     06F0BH          ; evaluate address operand (BLOAD/SAVE)
PARDEV  equ     06F15H          ; skip strong cassette devicecheck
CRDONZ  equ     07323H          ; newline to OUTDO if not at start of line
CRDO    equ     07328H          ; newline to OUTDO
M739A   equ     0739AH          ; quit loading & start (headloop/executing)
M7D17   equ     07D17H          ; continue start of MSX-BASIC without executing BASIC programs in ROM
M7D2F   equ     07D2FH          ; address initialize BASIC screen
M7D31   equ     07D31H          ; BASIC initscreen (without INITXT & CNSDFG)
M7E14   equ     07E14H          ; start MSX-BASIC program in ROM

RETRTN  equ     WRPRIM+6        ; address with a RET instruction, allways available


; FCB structure

; off   name    cp/m function           msx function

; +0    DR      drive                   drive
; +1,8  F1-F8   filename                filename
; +9,3  T1-T3   filetype                filetype
; +12   EX      extent                  extent
; +13   S1      reserved                fileattribute
; +14   S2      reserved                extent high byte / recordsize low byte (block)
; +15   RC      record count in extent  record count in extent / recordsize high byte (block)
; +16   AL      allocation              Filesize
; +20   AL      allocation              Date
; +22   AL      allocation              Time
; +24   AL      allocation              Devicecode
; +25   AL      allocation              Directoryentry Number
; +26   AL      allocation              Start Cluster
; +28   AL      allocation              Current Cluster
; +30   AL      allocation              Current Relative Cluster
; +32   CR      record in extent        record in extent
; +33,3 R0-R2   random access record    random access record
; +36   R3      not used                random access record when recordsize <64



        defb    "AB"
        defw    A576F
        defw    A6576
        defw    0
        defw    0
        defs    6

; disk hardware driver entries

T4010:  jp      DSKIO                   ; DSKIO entrypoint
T4013:  jp      DSKCHG                  ; DSKCHG entrypoint
T4016:  jp      GETDPB                  ; GETDPB entrypoint
T4019:  jp      CHOICE                  ; CHOICE entrypoint
T401C:  jp      DSKFMT                  ; DSKFMT entrypoint
T401F:  jp      MTOFF                   ; MTOFF entrypoint

; kernel entries

A4022:  jp      A5B3A                   ; start DiskBasic entrypoint
A4025:  scf                             ; format disk entrypoint (workarea must be supplied)
        jp      A60B1
A4029:  jp      A620D                   ; stop all disks entry point

; Unused code ??

        nop

GETSLT:
A402D:  jp      A5FB3                   ; get slotid entrypoint

;       Subroutine      get MSX-DOS system bottom
;       Inputs          -
;       Outputs         HL = lowest address used by the base MSX-DOS system

A4030:  ld      hl,(YF34B)
        ret

;       Subroutine      check if keyboardinput available
;       Inputs          -
;       Outputs         Zx set if no input, Zx reset if input, A = input

A4034:  push    ix
        ld      ix,BREAKX
        call    A40AB                   ; BREAKX BIOS call
        pop     ix                      ; CTRL-STOP pressed ?
        jr      nc,A404B                ; nope,
        ld      a,003H
        ld      (YF336),a               ; saved input available
        ld      (YF337),a               ; CTRL-C
        and     a                       ; flag NZ
        ret

A404B:  ld      a,(YF336)
        and     a                       ; saved input available ?
        ld      a,(YF337)
        ret     nz                      ; yep, return it (flag NZ)
        push    ix
        ld      ix,CHSNS
        call    A40AB                   ; CHSNS BIOS call
        pop     ix                      ; any chars in the keyboard buffer ?
        ret     z                       ; nope, quit (flag Z)
        ld      a,0FFH
        ld      (YF336),a               ; flag saved input available
        push    ix
        ld      ix,CHGET
        call    A40AB                   ; CHGET BIOS call
        pop     ix                      ; get char from keyboard buffer
        ld      (YF337),a               ; save char
        push    bc
        ld      b,000H
        inc     b
        pop     bc                      ; flag NZ
        ret

;       Subroutine      get keyboardinput
;       Inputs          -
;       Outputs         A = input

A4078:  push    hl
        ld      hl,YF336
        xor     a
        cp      (hl)                    ; saved input available ?
        ld      (hl),a                  ; not anymore!
        inc     hl
        ld      a,(hl)
        pop     hl
        ret     nz                      ; yep, return it
        push    ix
        ld      ix,CHGET
        call    A40AB                   ; CHGET BIOS call
        pop     ix                      ; get char
        ret

;       Subroutine      output to screen
;       Inputs          A = output
;       Outputs         -

A408F:  push    ix
        ld      ix,CHPUT
        call    A40AB                   ; CHPUT BIOS call
        pop     ix
        ret

;       Subroutine      output to printer
;       Inputs          A = output
;       Outputs         -

A409B:  push    ix
        ld      ix,LPTOUT
        call    A40AB                   ; LPTOUT BIOS call
        pop     ix
        ret

;       Subroutine      BDOS 00 (system reset)
;       Inputs          
;       Outputs         ________________________


A40A7:  ld      ix,READYR               ; restart BASIC

;       Subroutine      MSX-BIOS call
;       Inputs          IX = bios call, others depends on the bios call
;       Outputs         depends on the bios call

A40AB:  push    iy
        ld      iy,(EXPTBL-1+0)
        call    CALSLT
        ei
        pop     iy
        ret

;       Subroutine      check for and initialize clockchip
;       Inputs          -
;       Outputs         -

A40B8:  ld      a,13
        out     (0B4H),a
        ld      a,00AH
        out     (0B5H),a                ; alarm off, clock running, bank 2
        xor     a
        out     (0B4H),a                ; pos 0
        ld      b,00FH
A40C5:  in      a,(0B5H)                ; read data
        and     00FH
        xor     b
        out     (0B5H),a                ; change it and write back
        ld      c,a
        nop                             ; wait (orginal had 2x EX (SP),HL but by saving one byte all routine after this routine stay alligned)
        in      a,(0B5H)
        and     00FH
        cp      c                       ; correctly readback ?
        ret     nz                      ; nope, no clockchip!
        xor     b
        out     (0B5H),a                ; restore orginal data
        djnz    A40C5                   ; try all values
        ld      a,0FFH
        ld      (YF338),a               ; flag use clockchip
        ld      a,13
        out     (0B4H),a
        ld      a,009H
        out     (0B5H),a                ; alarm off, clock running, bank 1
        ld      a,10
        out     (0B4H),a                ; pos 10
        ld      a,1
        out     (0B5H),a                ; 24 hour system
        ld      a,13
        out     (0B4H),a
        xor     a
        out     (0B5H),a                ; alarm off, clock paused, bank 0
        ld      bc,00D00H
A40F8:  ld      a,c
        out     (0B4H),a
        in      a,(0B5H)
        push    af
        inc     c
        djnz    A40F8                   ; save time registers
        ld      a,14
        out     (0B4H),a
        xor     a
        out     (0B5H),a                ; clear testbits
        ld      b,00DH
A410A:  dec     c
        ld      a,c
        out     (0B4H),a
        pop     af
        out     (0B5H),a
        djnz    A410A                   ; restore time registers
        jr      A414E                   ; put clock in running mode

;       Subroutine      store date
;       Inputs          
;       Outputs         -

A4115:  ld      (YF33B),hl
        ld      a,(YF338)
        and     a                       ; use clockchip ?
        ret     z                       ; no, quit
        ld      a,(YF24A)
        ld      b,a
        ld      a,(YF249)
        ld      c,a
        ld      a,(YF248)
        ld      d,a                     ; current day
        ld      e,007H                  ; nibble 7 (date)
        call    A4159                   ; pause clock, select bank 0
        jr      A4142

;       Subroutine      store time
;       Inputs          
;       Outputs         -

A4130:  ld      a,(YF338)
        and     a                       ; use clockchip ?
        ret     z                       ; no, quit
        ld      e,000H                  ; nibble 0 (time)
        call    A4159                   ; pause clock, select bank 0
        ld      a,00FH
        out     (0B4H),a
        ld      a,002H
        out     (0B5H),a                ; time reset
A4142:  ld      h,d
        call    A4160                   ; write clockchip byte
        ld      h,c
        call    A4160                   ; write clockchip byte
        ld      h,b
        call    A4160                   ; write clockchip byte

;       Subroutine      put clockchip in running mode
;       Inputs          -
;       Outputs         -

A414E:  ld      a,13
        out     (0B4H),a
        in      a,(0B5H)
        or      008H
A4156:  out     (0B5H),a
        ret

;       Subroutine      pause clockchip and select bank 0
;       Inputs          -
;       Outputs         -

A4159:  call    A414E                   ; clock in running mode
        and     004H
        jr      A4156                   ; clock paused, select bank 0

;       Subroutine      write byte to clockchip
;       Inputs          H = data, E = nibblenumber
;       Outputs         E = updated nibblenumber (+2)

A4160:  xor     a
        ld      l,8
A4163:  rlc     h
        adc     a,a
        daa
        dec     l
        jr      nz,A4163                ; convert to BCD
        call    A4171
        rrca
        rrca
        rrca
        rrca
A4171:  push    af
        ld      a,e
        inc     e
        out     (0B4H),a
        pop     af
        jr      A4156

;       Subroutine      get date and time
;       Inputs          
;       Outputs         Cx set if from clockchip,

A4179:  ld      a,(YF338)
        and     a                       ; use clockchip ?
        ld      b,a
        ld      c,a
        ld      d,a
        ld      e,a                     ; 00:00:00
        ld      hl,(YF33B)              ; days since 1-1-1980
        ret     z                       ; no clockchip, quit
        call    A4159                   ; clock paused, select bank 0
        ld      e,12+1
        call    A41AD                   ; read byte from clockchip
        call    A5523                   ; setup days in februari
        call    A41AD                   ; read byte from clockchip
        ld      (YF249),a               ; current month
        call    A41AD                   ; read byte from clockchip
        ld      (YF248),a               ; current day
        dec     e                       ; nibble 5
        call    A41AD                   ; read byte from clockchip
        ld      b,a
        call    A41AD                   ; read byte from clockchip
        ld      c,a
        call    A41AD                   ; read byte from clockchip
        call    A414E                   ; clock in running mode
        scf
        ret

;       Subroutine      read byte from clockchip
;       Inputs          E = nibblenumber+1
;       Outputs         E = updated nibblenumber (-2), A = data

A41AD:  xor     a
        call    A41B5
        add     a,a
        add     a,a
        add     a,d
        add     a,a
A41B5:  ld      d,a
        dec     e
        ld      a,e
        out     (0B4H),a
        in      a,(0B5H)
        and     00FH
        add     a,d
        ld      d,a
        ret

; Identification string (not used)

S41C1:  defb    " MSX-DOS ver. 2.2 Copyright 1984 by Microsoft "

;       Subroutine      BDOS 0C (return version number)
;       Inputs          
;       Outputs         ________________________


A41EF:  ld      b,000H                  ; machinetype 8080, plain CP/M
        ld      a,022H                  ; CP/M version 2.2
        ret

;       Subroutine      get FAT entry content
;       Inputs          HL = clusternumber, IX = pointer to DPB
;       Outputs         HL = clusterentry content, Zx set if entry is free, DE = pointer to FAT buffer

A41F4:  ld      e,(ix+19)
        ld      d,(ix+20)               ; pointer to FAT buffer of drive
A41FA:  call    XF252
        push    de
        ld      e,l
        ld      d,h
        srl     h
        rr      l
        rra
        add     hl,de
        pop     de
        add     hl,de
        rla
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        jr      nc,A421A
        srl     h
        rra
        srl     h
        rra
        srl     h
        rra
        srl     h
        rra
A421A:  ld      l,a
        ld      a,h
        and     00FH
        ld      h,a
        or      l
        ret

;       Subroutine      set FAT entry content
;       Inputs          HL = clusternumber, DE = pointer to FAT buffer, BC = clusterentry content
;       Outputs         -

A4221:  push    de
        ld      e,l
        ld      d,h
        srl     h
        rr      l
        rra
        add     hl,de
        pop     de
        add     hl,de
        rla
        jr      nc,A4247
        sla     c
        rl      b
        sla     c
        rl      b
        sla     c
        rl      b
        sla     c
        rl      b
        ld      a,(hl)
        and     00FH
        or      c
        ld      (hl),a
        inc     hl
        ld      (hl),b
        ret

A4247:  ld      (hl),c
        inc     hl
        ld      a,(hl)
        and     0F0H
        or      b
        ld      (hl),a
        ret

;       Subroutine      compare with filename1
;       Inputs          HL = pointer to buffer, B = size
;       Outputs         Zx set if equal

A424F:  ld      de,YF2B9

;       Subroutine      compare
;       Inputs          DE = pointer to buffer1, HL = pointer to buffer2, B = size
;       Outputs         Zx set if equal

A4252:  ld      a,(de)
        cp      (hl)
        inc     hl
        inc     de
        ret     nz
        djnz    A4252
        ret

;       Subroutine      check if devicename
;       Inputs          
;       Outputs         ________________________

A425A:  call    XF255
        ld      hl,YF1F7                ; table with devicenames
        ld      c,5                     ; 5 devices
A4262:  ld      b,4                     ; only check 4 bytes (because devicenames are only 4 chars long)
        call    A424F                   ; compare with filename1
        jr      nz,A4298                ; not this device, try the next
        ld      b,4
A426B:  ld      a,(de)
        inc     de
        cp      " "
        jr      nz,A42A3                ; last 4 bytes of filename not spaces, not a device
        djnz    A426B
        ld      a,c
        neg
        ld      (YF20B+11),a            ; devicecode
        ld      hl,YF2B9
        ld      de,YF20B
        ld      bc,4
        ldir                            ; copy of devicename
        call    A5496                   ; get time and date (dirformat)
        ld      (YF20B+24),bc
        ld      (YF20B+22),de
        ld      hl,YF20B
        push    hl
        pop     iy
        or      001H                    ; Cx reset, Zx reset
        ret

A4298:  dec     b
        ld      a,l
        add     a,b
        ld      l,a
        ld      a,h
        adc     a,000H
        ld      h,a
        dec     c
        jr      nz,A4262
                                        ; Zx set
A42A3:  scf
        ret

;       Subroutine      validate FCB, clear S2 and find direntry
;       Inputs          
;       Outputs         ________________________


A42A5:  push    de
        ld      hl,14
        add     hl,de
        ld      (hl),0                  ; FCB S2 (extent high byte)
        call    A42B1                   ; validate FCB drive and filename and find direntry
        pop     de
        ret

A42B1:  call    A440E                   ; validate FCB drive and filename
        ret     c                       ; invalid, quit

;       Subroutine      find first directoryentry
;       Inputs          
;       Outputs         ________________________


A42B5:  call    A425A                   ; check if devicename
        ret     nc                      ; yep, quit with pointer to fake device direntry
        call    A44D3                   ; reset direntry search and get latest FAT

;       Subroutine      find next directoryentry
;       Inputs          
;       Outputs         ________________________

A42BC:  call    XF258
        call    A430E                   ; get next direntry
        ret     c                       ; no more, quit
A42C3:  ld      a,(hl)
        or      a
        jr      z,A42FC                 ; unused entry,
        cp      0E5H
        jr      z,A42FC                 ; deleted entry,
        push    hl
        ld      b,11
        ld      de,YF2B9
A42D1:  call    A4252                   ; compare with fcb filename
        jr      z,A42DC                 ; equal, found!
        cp      "?"
        jr      nz,A42F5                ; on difference no wildcard, try next
        djnz    A42D1                   ; wildcard pos ignored, check rest
A42DC:  pop     hl
        push    hl
        pop     iy
        ld      a,(YF2C4)
        xor     080H
        bit     7,a
        ret     z                       ; orginal FCB DR byte had b7 set, ignore direntryattribute
        ld      a,(iy+11)
        and     01EH
        ret     z                       ; files with archive or read-only bit set are ok, quit
        ld      a,(YF2DC)
        or      a                       ; include special fileattribute flag set ?
        ret     nz                      ; yep, every direntry is ok, quit
        jr      A42F6

A42F5:  pop     hl
A42F6:  call    A4348                   ; get next direntry (while searching)
        jr      nc,A42C3                ; ok, check it
        ret

A42FC:  ld      a,(YF2FE)
        inc     a                       ; already found a free direntry ?
        jr      nz,A4308
        ld      a,(YF2B8)
        ld      (YF2FE),a               ; nope, register it
A4308:  ld      a,(hl)
        or      a                       ; unused direntry ?
        jr      nz,A42F6                ; nope, the search goes on!
        scf
        ret

;       Subroutine      get next direntry (at start of search)
;       Inputs          
;       Outputs         ________________________

A430E:  ld      a,(YF2B8)
        inc     a
        cp      (ix+11)                 ; last direntry ?
        jr      nc,A4367                ; yep, update directory of disk when needed and quit

;       Subroutine      get direntry
;       Inputs          
;       Outputs         ________________________

A4317:  call    XF25B
        ld      (YF2B8),a
        ld      c,a
        and     (ix+4)                  ; dirmask
        ld      l,a
        ld      h,0
        add     hl,hl
        add     hl,hl
        add     hl,hl
        add     hl,hl
        add     hl,hl
        ld      de,(YF351)              ; dirsector buffer
        add     hl,de
        ld      b,(ix+5)                ; dirshift
A4331:  srl     c
        djnz    A4331
        ld      a,(YF245)
        cp      c                       ; same as dirsector currently in buffer ?
        jr      nz,A4342                ; nope, go get it
        ld      a,(YF246)
        cp      (ix+0)                  ; same driveid as dirsector buffer owner ?
        ret     z                       ; yep, do nothing
A4342:  push    hl
        call    A46A4                   ; read dirsector
        pop     hl
        ret

;       Subroutine      get next direntry (while searching)
;       Inputs          
;       Outputs         ________________________

A4348:  call    XF25E
        ld      a,(YF2B8)
        inc     a
        cp      (ix+11)                 ; last direntry ?
        jr      nc,A4367                ; yep, update directory of disk when needed and quit
        ld      (YF2B8),a
        ld      de,00020H
        add     hl,de
        and     (ix+4)                  ; dirmask
        ret     nz
        inc     c
        call    A46A4                   ; read dirsector
        ld      hl,(YF351)              ; dirsector buffer
        ret

;       Subroutine      at end of directory
;       Inputs          
;       Outputs         ________________________

A4367:  call    A4743                   ; flush directory buffer
        scf
        ret

;       Subroutine      BDOS 13 (delete file)
;       Inputs          
;       Outputs         ________________________

A436C:  call    A440E                   ; validate FCB drive and filename
        call    nc,A42B5                ; valid, find first directoryentry
        ld      a,0FFH
        ret     c                       ; invalid or not found, quit with error
        ret     nz                      ; device, quit with error
A4376:  ld      a,0E5H
        ld      (YF23C),a               ; flag directory buffer changed
        ld      (hl),a                  ; deleted direntry
        ld      l,(iy+26)
        ld      h,(iy+27)
        ld      a,h                     ; file has start cluster ?
        or      l
        call    nz,A4F9B                ; yep, release cluster chain
        call    A42BC                   ; find next directoryentry
        jr      nc,A4376                ; found, delete next file
        call    A4403                   ; update directory of disk (SHOULD BE: CALL A4748)
        jp      A45CF                   ; write FAT buffer (SHOULD BE: JP A45C4, flush FAT buffer)

;       Subroutine      BDOS 17 (rename file)
;       Inputs          
;       Outputs         ________________________

A4392:  call    A440E                   ; validate FCB drive and filename
        jr      c,A440B                 ; invalid, quit with error
        ld      de,00005H
        add     hl,de                   ; to new filename
        ld      de,YF2C5                ; new filenamebuffer
        call    XF1F4                   ; validate FCB filename (new filename)
        call    nc,A42B5                ; new filename valid, find first directoryentry
        jr      c,A440B                 ; invalid or not found, quit with error
        jr      nz,A440B
        ld      hl,YF2B9
        ld      de,YF2D0
        ld      bc,11+1
        ldir                            ; save filename (search specifier) + orginal DR byte
A43B3:  ld      hl,YF2C5                ; new filename
        ld      de,YF2B9
        ld      b,11
A43BB:  ld      a,(hl)
        cp      "?"                     ; wildcard char ?
        jr      nz,A43C3                ; nope, use the char of the new filename
        ld      a,(iy+0)                ; yep, use the char of the orginal filename
A43C3:  ld      (de),a
        inc     hl
        inc     de
        inc     iy
        djnz    A43BB
        ld      a,080H
        ld      (de),a                  ; orginal DR byte b7 set (ignore fileattribute)
        call    A425A                   ; check if devicename
        jr      nc,A4408                ; yep, end rename with error
        ld      a,(YF2B8)
        push    af
        ld      a,0FFH
        ld      (YF2B8),a               ; flag direntry search start at the begin
        call    A42BC                   ; find next directoryentry
        pop     bc
        jr      nc,A4408                ; found, so resulting filename does already exist. end rename with error
        ld      a,b
        call    A4317                   ; get direntry which get renamed
        ex      de,hl
        ld      hl,YF2B9
        ld      bc,11
        ldir                            ; replace filename with new one
        ld      a,0FFH
        ld      (YF23C),a               ; flag directory buffer changed
        ld      hl,YF2D0
        ld      de,YF2B9
        ld      bc,11+1
        ldir                            ; restore filename (search specifier) + orginal DR byte
        call    A42BC                   ; find next directoryentry
        jr      nc,A43B3                ; found, rename next file
A4403:  call    A4743                   ; flush directory buffer
        xor     a                       ; no error
        ret

A4408:  call    A4743                   ; flush directory buffer
A440B:  ld      a,0FFH                  ; error
        ret

;       Subroutine      validate FCB drive and filename
;       Inputs          
;       Outputs         ________________________

A440E:  call    XF261
        xor     a
        ld      (YF2DC),a               ; do not include special fileattributes
        ex      de,hl
        ld      a,(hl)
        inc     hl
        ld      (YF2C4),a               ; save FCB DR byte
        and     00FH                    ; only use b3-b0 for drive
        call    A4427                   ; validate fcb driveid
        ret     c
        ld      de,YF2B9
        jp      XF1F4                   ; validate FCB filename

;       Subroutine      Validate driveid (FCB style)
;       Inputs          A = driveid
;       Outputs         ________________________

A4427:  ld      c,a
        ld      a,(YF347)
        cp      c
        ret     c
        ld      a,c
        dec     a
        jp      p,A4435
        ld      a,(YF247)               ; default driveid
A4435:  ld      (YF2E1),a               ; set current driveid
        ret

;       Subroutine      get max record and extent
;       Inputs          
;       Outputs         ________________________

A4439:  ld      a,(iy+31)
        or      a
        jr      nz,A445E                ; filesize > 16777215, use max value
        ld      a,(iy+28)
        ld      c,(iy+29)
        ld      b,(iy+30)
        add     a,a
        rl      c
        rl      b                       ; number of records (128 bytes)
        jr      c,A445E                 ; >65535, use max value
        or      a                       ; is filesize a multiply of 128 ?
        jr      z,A4457
        inc     bc                      ; nope, increase the recordnumber
        ld      a,b
        or      c                       ; does that fit ?
        jr      z,A445E                 ; nope, use max value
A4457:  ld      a,c
        res     7,c                     ; c = max recordnumber (0-127)
        add     a,a
        rl      b                       ; b = max extent
        ret     nc                      ; does fit, quit
A445E:  ld      bc,0FF7FH               ; extent 255, record 127
        ret

;       Subroutine      BDOS 0F (open file)
;       Inputs          
;       Outputs         ________________________

A4462:  call    A42A5                   ; validate FCB, clear S2 and find direntry
        jr      c,A440B                 ; error, quit with error
        call    A4439                   ; get max record and extent
        ld      a,(YF30C)               ; original FCB EX byte
        inc     b                       ; ?? correct for large files (filesize > 4177919 where extend is 0FFH)
        cp      b                       ; is extent of file big enough ?
        jr      nc,A440B                ; nope, quit with error
A4471:  call    XF264
        ex      de,hl
        ld      bc,0000FH
        add     hl,bc
        call    A4439                   ; get max record and extent
        ld      a,(YF30C)
        cp      b                       ; orginal FCB EX byte same as max extent ?
        jr      z,A4488                 ; same, use RC=max recordnumber (means extent is not full)
        ld      c,080H
        jr      c,A4488                 ; smaller, use RC=128 (means extend is full)
        ld      c,000H                  ; bigger, use RC=0 (means extend is empty)
A4488:  ld      (hl),c                  ; RC
        inc     hl
        ex      de,hl
        ld      bc,0001CH
        add     hl,bc
        ld      c,004H
        ldir                            ; copy filesize
        ld      bc,0FFF8H
        add     hl,bc
        ldi
        ldi                             ; creation date
        ld      c,0FCH
        add     hl,bc
        ldi
        ldi                             ; creation time
        ld      a,(iy+11)
        bit     7,a
        jr      nz,A44AE                ; device,
        ld      a,(ix+0)                ; driveid
        or      040H                    ; flag diskfile unchanged
A44AE:  ld      (de),a                  ; devicecode
        inc     de
        ld      a,(YF2B8)
        ld      (de),a                  ; direntry number
        inc     de
        ld      a,(iy+26)
        ld      (de),a
        inc     de
        inc     de
        ld      (de),a
        dec     de
        ld      a,(iy+27)
        ld      (de),a
        inc     de
        inc     de
        ld      (de),a                  ; start cluster and last cluster accessed
        inc     de
        xor     a
        ld      (de),a
        inc     de
        ld      (de),a                  ; last cluster accessed, relative
        ret

;       Subroutine      handle DSKCHG error
;       Inputs          
;       Outputs         ________________________

A44CA:  ld      c,a
        ld      a,(YF2E1)               ; current driveid
        call    A470A                   ; start diskerror handler
        jr      A44DB                   ; get latest FAT (try again)

;       Subroutine      reset direntry search and get latest FAT
;       Inputs          
;       Outputs         ________________________

A44D3:  ld      a,0FFH
        ld      (YF2B8),a               ; invalid latest direntry (search from the begin)
        ld      (YF2FE),a               ; not found a free direntry

;       Subroutine      get latest FAT
;       Inputs          
;       Outputs         ________________________

A44DB:  call    XF267
        call    A4555                   ; get pointer to DPB of current drive
        ld      a,(YF2E1)               ; current driveid
        ld      c,(ix+1)                ; mediadesciptor
        ld      b,0
        or      a
        call    A6067                   ; DSKCHG
        jr      c,A44CA                 ; error,
        call    A4536                   ; update DPBTBL entry current drive
        ld      l,(ix+19)
        ld      h,(ix+20)
        dec     hl
        ld      a,b                     ; DSKCHG status
        or      (hl)                    ; combined with the FAT buffer status
        ld      a,(YF2E1)               ; current driveid
        ld      hl,(YF241)
        jp      m,A450A                 ; FAT buffer invalid OR diskchange unknown, read the FAT
        ret     nz                      ; FAT buffer changed OR disk unchanged, do not read the FAT and quit
        cp      l                       ; current drive same as datasector buffer owner ?
        jr      nz,A4516                ; nope, read the FAT
        dec     h                       ; datasector buffer changed ?
        ret     z                       ; yep, do not read the FAT and quit
A450A:  sub     l                       ; current drive same as datasector buffer owner ?
        jr      nz,A4516                ; nope, leave the datasector buffer alone
        ld      l,a
        ld      h,a

; VERSION 1.1 CHANGE
; ld (YF23F),hl is removed, because it is not explicit needed
; room is used later on to fix the FAT reading bug when one of the other FAT copies is used (when first FAT read generates a error)

        dec     l
        ld      (YF241),hl              ; invalid datasector buffer
A4516:  ld      a,0FFH
        ld      (YF246),a               ; invalid dirsector buffer
        call    A45FA                   ; get FAT parameters
        dec     hl
        ld      (hl),0                  ; FAT buffer unchanged
        inc     hl
A4522:  push    af
        call    A46D7                   ; read FAT sectors
        jr      c,A4541                 ; error, try the next FAT copy
        pop     af
A4529:  ld      b,(hl)                  ; mediabyte of FAT sector
        ld      a,(YF2E1)               ; current driveid
        ld      c,(ix+1)                ; mediadescriptor
        push    ix
        pop     hl
        call    A606F                   ; GETDPB
A4536:  push    hl
        pop     ix
        ex      de,hl
        call    A4563                   ; get DPBTBL entry of current drive
        ld      (hl),e
        inc     hl
        ld      (hl),d
        ret

A4541:  ld      a,e
        add     a,c
        ld      e,a
        jr      nc,A4547
        inc     d
A4547:  pop     af                      ; adjust first FAT sector to the first FAT sector of the next FAT copy

; VERSION 1.1 BUGFIX
; Begin of change

        ld      b,c                     ; restore sectors per FAT (B is destroyed by the DSKIO)

; End of change
 
        dec     a
        jr      nz,A4522                ; there is a other FAT copy, try that one
        call    A45FA                   ; get FAT parameters (so the first FAT copy is used)
        push    hl
        call    A46C5                   ; read FAT sectors with DOS error handling
        pop     hl
        jr      A4529                   ; use FAT buffer

;       Subroutine      get pointer to DPB of current drive
;       Inputs          
;       Outputs         HL = IX = pointer to DPB

A4555:  call    XF26A
        call    A4563                   ; get DPBTBL entry of current drive
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        push    hl
        pop     ix
        ret

;       Subroutine      get DPBTBL entry of current drive
;       Inputs          HL = address of pointer
;       Outputs         ________________________

A4563:  ld      a,(YF2E1)               ; current driveid
        ld      hl,YF355
        add     a,a
        add     a,l
        ld      l,a
        ret     nc
        inc     h
        ret

; CHANGED, TO KEEP ROUTINES ALIGNED WITH PREVIOUS VERSION

        defs    0456FH-$,0

;       Subroutine      BDOS 10 (close file)
;       Inputs          
;       Outputs         ________________________

A456F:  push    de
        pop     iy
        call    A440E                   ; validate FCB drive and filename
        ld      a,0FFH
        ret     c                       ; invalid, quit with error
        ld      a,(iy+24)
        and     0C0H
        ld      a,0                     ; ok
        ret     nz                      ; device OR unchanged diskfile, quit
        ld      a,(YF2E1)               ; current driveid
        ld      hl,(YF241)
        cp      l                       ; same drive as owner datasector buffer ?
        call    z,A472D                 ; yep, flush datasector buffer
        call    A4555                   ; get pointer to DPB of current drive
        ld      a,(iy+25)               ; direntrynumber
        call    A4317                   ; get direntry
        ld      b,11
        call    A424F                   ; compare with filename1
        jr      nz,A45EE                ; not the same, make FAT buffer unchanged and quit with error
        push    iy
        pop     de
        ld      c,00BH
        add     hl,bc
        ex      de,hl
        ld      c,016H
        add     hl,bc
        ldi
        ldi
        ld      bc,0FFFCH
        add     hl,bc
        ldi
        ldi
        ld      bc,00004H
        add     hl,bc
        ldi
        ldi
        ld      bc,0FFF4H
        add     hl,bc
        ld      bc,00004H
        ldir
        call    A4748                   ; update directory of disk

;       Subroutine      flush FAT buffer
;       Inputs          
;       Outputs         ________________________

A45C4:  ld      l,(ix+19)
        ld      h,(ix+20)
        dec     hl
        ld      a,(hl)
        cp      1                       ; FAT buffer changed ?
        ret     nz                      ; nope, quit (?? return error if FAT buffer invalid)

;       Subroutine      write FAT buffer
;       Inputs          
;       Outputs         ________________________

A45CF:  call    XF26D
        call    A45FA                   ; get FAT parameters
        dec     hl
        ld      (hl),0                  ; FAT buffer unchanged
        inc     hl
A45D9:  push    af
        push    de
        push    bc
        push    hl
        call    A4755                   ; write FAT sectors with DOS error handling
        pop     hl
        pop     bc
        pop     de
        ld      a,e
        add     a,b
        ld      e,a
        jr      nc,A45E9
        inc     d                       ; to start sector of the next FAT
A45E9:  pop     af
        dec     a
        jr      nz,A45D9                ; write next FAT
        ret

;       Subroutine      make FAT buffer unchanged and quit with error
;       Inputs          
;       Outputs         ________________________

A45EE:  ld      l,(ix+19)
        ld      h,(ix+20)
        dec     hl
        ld      (hl),0                  ; FAT buffer unchanged
        ld      a,0FFH                  ; error
        ret

;       Subroutine      get FAT parameters
;       Inputs          IX = pointer to DPB
;       Outputs         A = number of FATs, DE = first FAT sector, B = number sectors per FAT, HL = pointer to FAT buffer

A45FA:  ld      a,(ix+10)               ; number of FATs
        ld      l,(ix+19)
        ld      h,(ix+20)               ; pointer to FAT buffer
        ld      b,(ix+16)               ; number of sectors per FAT
        ld      e,(ix+8)
        ld      d,(ix+9)                ; first FAT sector
        ret

;       Subroutine      get dir parameters
;       Inputs          IX = pointer to DPB, A = relative dirsector, DE = first dirsector
;       Outputs         DE = dirsector, B = 1, HL = pointer to dirsector buffer

A460D:  add     a,(ix+17)
        ld      e,a
        ld      d,(ix+18)
        jr      nc,A4617
        inc     d                       ; + first dir sector
A4617:  ld      hl,(YF351)              ; dirsector buffer
        ld      b,1                     ; 1 sector
        ret

;       Subroutine      BDOS 16 (create file)
;       Inputs          
;       Outputs         ________________________

A461D:  push    de
T461E:  call    A440E                   ; validate FCB drive and filename
        jr      c,A464D                 ; invalid, quit with error
        inc     hl
        inc     hl
        ld      (hl),0                  ; clear S2 byte
        ld      hl,YF2B9
        ld      a,"?"
        ld      bc,11
        cpir                            ; wildcard char in filename ?
        jr      z,A464D                 ; yep, quit with error
        call    A42B5                   ; find first directoryentry
        jr      nc,A4651                ; found, special actions for existing file/device
        ld      a,(YF2FE)
        cp      0FFH                    ; found free direntry ?
        jr      z,A464D                 ; nope, quit with error (directory is full)
        call    A4317                   ; get direntry
        push    hl
        pop     iy
        jr      A4669                   ; setup direntry

A4647:  bit     7,(iy+11)
        jr      nz,A469D                ; device, treat as open file
                                        ; file with special fileattribute, quit with error
A464D:  pop     de
        ld      a,0FFH
        ret

A4651:  jr      nz,A4647                ; device or file with special fileattribute,
        ld      a,(YF30C)               ; orginal FCB EX byte
        or      a
        jr      nz,A469D                ; is not zero, just open the file
        ld      l,(iy+26)
        ld      h,(iy+27)
        ld      a,h
        or      l                       ; has start cluster ?
        jr      z,A4669                 ; nop,
        call    A4F9B                   ; release cluster chain
        call    A45CF                   ; write FAT buffer
A4669:  push    iy
        pop     de
        ld      hl,YF2B9
        ld      bc,11
        ldir                            ; copy filename in FCB to direntry
        ld      a,(hl)
        rla
        ld      a,000H
        jr      nc,A467C                ; b7 DR byte reset, ordinary file
        ld      a,006H                  ; b7 DR byte set, hidden system file
A467C:  ld      (de),a
        inc     de
        ex      de,hl
        ld      b,10
        xor     a
A4682:  ld      (hl),a
        inc     hl
        djnz    A4682                   ; clear unused bytes direntry
        push    hl
        call    A5496                   ; get time and date (dirformat)
        pop     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        inc     hl
        ld      (hl),c
        inc     hl
        ld      (hl),b
        inc     hl                      ; fill in time and date in direntry
        xor     a
        ld      b,2+4
A4696:  ld      (hl),a
        inc     hl
        djnz    A4696                   ; fill in no first cluster, filesize 0 in direntry
        call    A4748                   ; update directory of disk

A469D:  push    iy
        pop     hl
        pop     de
        jp      A4471                   ; continue with open file

;       Subroutine      read dirsector
;       Inputs          C = relative dir sector
;       Outputs         ________________________

A46A4:  push    bc
        call    A4743                   ; flush directory buffer
        pop     bc
        ld      b,(ix+0)                ; driveid
        ld      (YF246-1),bc            ; set driveid and sector dirsector buffer
        push    bc
        ld      a,c                     ; relative dirsector
        call    A460D                   ; setup dirsector parameters
        call    A46C5                   ; read dirsector with DOS error handling
        pop     bc
        ret

;       Subroutine      BDOS 2F (read logical sector)
;       Inputs          
;       Outputs         ________________________

A46BA:  ld      b,h
        ld      a,l
        ld      (YF2E1),a               ; set current driveid
        call    A4555                   ; get pointer to DPB of current drive
        ld      hl,(YF23D)              ; transferaddress

;       Subroutine      read sectors with DOS error handling
;       Inputs          
;       Outputs         ________________________

A46C5:  call    XF270
        xor     a
        ld      (YF2FF),a               ; flag read disk operation
        call    A46D7                   ; read sector
        ret     nc                      ; no error, quit
        call    A46E8                   ; adjust parameters to restart at error sector and start diskerror handler
        dec     a
        jr      z,A46C5                 ; RETRY, try again
        ret                             ; IGNORE, quit

;       Subroutine      read sectors
;       Inputs          
;       Outputs         ________________________

A46D7:  ld      a,(ix+0)                ; driveid
        ld      c,(ix+1)                ; mediadescriptor
        push    hl
        push    de
        push    bc
        call    A6052                   ; read disksector
        pop     de
        ld      c,d
        pop     de
        pop     hl
        ret

;       Subroutine      adjust parameters to restart at error sector and start diskerror handler
;       Inputs          
;       Outputs         ________________________

A46E8:  push    af
        ld      a,c
        sub     b
        ld      c,a
        push    bc
        ld      b,000H
        ex      de,hl
        add     hl,bc
        push    hl
        push    de
        ld      e,(ix+2)
        ld      d,(ix+3)                ; sectorsize
        call    A4916                   ; multiply
        pop     hl
        pop     de
        add     hl,bc
        pop     bc
        pop     af
        ld      c,a
        ld      a,(YF2FF)               ; type of diskoperation
        or      c
        ld      c,a
        ld      a,(ix+0)                ; driveid

;       Subroutine      start diskerror handler
;       Inputs          
;       Outputs         ________________________

A470A:  call    XF273
        push    bc
        push    de
        push    hl
        ld      hl,(YF323)
        call    XF1E8                   ; start diskerror handler in DOS memory
        ld      a,c                     ; requested action
        pop     hl
        pop     de
        pop     bc
        cp      2
        ret     nz
        jp      XF1E2                   ; Warm boot

;       Subroutine      BDOS 30 (write logical sector)
;       Inputs          
;       Outputs         ________________________

A4720:  ld      b,h
        ld      a,l
        ld      (YF2E1),a               ; set current driveid
        call    A4555                   ; get pointer to DPB of current drive
        ld      hl,(YF23D)              ; transferaddress
        jr      A4755                   ; write sectors with DOS error handling

;       Subroutine      flush datasector buffer
;       Inputs          
;       Outputs         ________________________

A472D:  ld      hl,YF242
        xor     a
        cp      (hl)                    ; datasector buffer changed ?
        ld      (hl),a                  ; now it is unchanged
        ret     z                       ; nope, quit
        ld      ix,(YF243)              ; saved DPB pointer
        ld      hl,(YF34F)              ; datasector buffer
        ld      b,1                     ; 1 sector
        ld      de,(YF23F)              ; sectornumber of datasector buffer
        jr      A4755                   ; write sector with DOS error handling

;       Subroutine      flush directory buffer
;       Inputs          
;       Outputs         ________________________

A4743:  ld      a,(YF23C)
        or      a                       ; directory buffer changed ?
        ret     z                       ; nope, quit

;       Subroutine      write dirsector buffer
;       Inputs          
;       Outputs         ________________________

A4748:  call    XF276
        xor     a
        ld      (YF23C),a               ; directory buffer unchanged
        ld      a,(YF245)               ; current dirsector (offset)
        call    A460D                   ; setup dirsector parameters

;       Subroutine      write sectors with DOS error handling
;       Inputs          
;       Outputs         ________________________

A4755:  call    XF279
        ld      a,1
        ld      (YF2FF),a               ; flag write disk operation
        ld      a,(ix+0)                ; driveid
        ld      c,(ix+1)                ; mediadescriptor
        push    hl
        push    de
        push    bc
        call    A6054                   ; write disksector
        pop     de
        ld      c,d
        pop     de
        pop     hl
        ret     nc                      ; no error, quit
        call    A46E8                   ; adjust parameters to restart at error sector and start diskerror handler
        dec     a
        jr      z,A4755                 ; RETRY, try again
        ret                             ; IGNORE, quit

;       Subroutine      BDOS 14 (read next record)
;       Inputs          
;       Outputs         ________________________

A4775:  call    A4EF8                   ; get recordnumber from CR,EX and S2 fields
        call    A4B23                   ; read record
        jr      A4783                   ; update sequencial fields

;       Subroutine      BDOS 15 (write next record)
;       Inputs          
;       Outputs         ________________________

A477D:  call    A4EF8                   ; get recordnumber from CR,EX and S2 fields
        call    A4CA3                   ; write record
A4783:  call    A486A                   ; increase recordnumber if something was read/written
        jr      A479C                   ; update CR,EX and S2 field

;       Subroutine      BDOS 21 (random access read record)
;       Inputs          
;       Outputs         ________________________

A4788:  call    A4857                   ; get recordnumber from Rx fields, 1 record
        call    A4B23                   ; read record
        jr      A4799                   ; update Rx, CR,EX and S2 field

A4790:  push    iy
        pop     de

;       Subroutine      BDOS 22 (random access write record)
;       Inputs          
;       Outputs         ________________________

A4793:  call    A4857                   ; get recordnumber from Rx fields, 1 record
        call    A4CA3                   ; write record
A4799:  call    A4844                   ; update Rx fields

A479C:  ld      a,l
        and     07FH
        ld      (iy+32),a               ; CR
        sla     l
        rl      h
        ld      (iy+12),h               ; S2
        rl      e
        ld      (iy+14),e               ; EX
        ld      a,(YF2DE)               ; result recordoperation
        ret

;       Subroutine      BDOS 27 (MSXDOS random block read)
;       Inputs          
;       Outputs         ________________________

A47B2:  xor     a
        ld      (YF306),a               ; no CP/M call
        call    A485A                   ; get random record number
        call    A4B23                   ; read record(s)
        jr      A47C8

;       Subroutine      BDOS 26 (MSXDOS random block write)
;       Inputs          
;       Outputs         ________________________

A47BE:  xor     a
        ld      (YF306),a               ; no CP/M call
        call    A485A                   ; get random record number
        call    A4CA3                   ; write record(s)
A47C8:  call    A486A                   ; increase recordnumber if something was read/written
        call    A4844                   ; update Rx fields
        ld      l,c
        ld      h,b
        ret

;       Subroutine      BDOS 28 (write random with zero fill)
;       Inputs          
;       Outputs         ________________________

A47D1:  push    de
        pop     iy
        ld      a,(iy+16)
        ld      c,(iy+17)
        ld      b,(iy+18)
        ld      e,(iy+19)
        add     a,a
        rl      c
        rl      b
        rl      e                       ; convert filesize to a random record number
        or      a                       ; was filesize a multiply of 128 ?
        jr      z,A47F0
        inc     bc
        ld      a,b
        or      c
        jr      nz,A47F0
        inc     e                       ; no, increase random record number

; the following code depends on the fact that CP/M 2.2 only uses the R0 and R1 field
; code only works when the random record (filesize) is within 65536 records of the random record (fcb)

A47F0:  ld      l,(iy+33)
        ld      h,(iy+34)               ; R1 and R0
        sbc     hl,bc
        jr      z,A4790                 ; exact at end of file, random access write record and quit
        ld      a,(iy+35)
        sbc     a,e                     ; before end of file ?
        jr      c,A4790                 ; yep, random access write record and quit
        push    hl                      ; save number of gap records
        call    A4790                   ; random access write record (gap in filled with garbage)
        pop     de
        or      a
        ret     nz                      ; error, quit

; now the gap is filled. dirsector buffer is used for the zero filled record
; neat code should first flush the dirsector buffer, but this is ommited

        ld      hl,(YF23D)              ; transferaddress
        push    hl
        ld      hl,(YF351)
        ld      (YF23D),hl              ; tempory use dirsector buffer
        ld      b,128
A4813:  ld      (hl),a
        inc     hl
        djnz    A4813                   ; create a zero filed random record
        dec     a
        ld      (YF246),a               ; invalid dirsector buffer
        ld      l,(iy+33)
        ld      h,(iy+34)
        sbc     hl,de
        ld      c,l
        ld      b,h
        ex      de,hl
        ld      d,000H
        ld      a,(iy+35)
        sbc     a,d
        ld      e,a                     ; start record of gap
A482D:  push    hl
        ld      hl,1
        call    A4CA3                   ; write record
        call    A486A                   ; increase recordnumber if something was writen
        ld      c,l
        ld      b,h
        pop     hl
        dec     hl
        ld      a,h
        or      l
        jr      nz,A482D                ; next record
        pop     hl
        ld      (YF23D),hl              ; restore transferaddress
        ret

A4844:  ld      a,(YF2DE)               ; result recordoperation
        ld      (iy+33),l
        ld      (iy+34),h
        ld      (iy+35),e
        inc     d
        dec     d
        ret     z
        ld      (iy+36),d
        ret

A4857:  ld      hl,1                    ; 1 record
A485A:  push    de
        pop     iy
        ld      c,(iy+33)               ; R0
        ld      b,(iy+34)               ; R1
        ld      e,(iy+35)               ; R2
        ld      d,(iy+36)               ; R3
        ret

A486A:  ret     z
        inc     hl
        ld      a,h
        or      l
        ret     nz
        inc     de
        ret

A4871:  pop     hl
        ld      l,c
        ld      h,b
        ld      a,1
        ld      (YF2DE),a               ; error in recordoperation
        xor     a
        ld      c,a
        ld      b,a
        ret

A487D:  ld      (YF2E8),hl              ; number of records requested
        ld      (YF2E4+0),bc
        ld      (YF2E4+2),de            ; startrecord
        ld      a,(iy+0)
        call    A4427                   ; validate fcb driveid
        jr      c,A4871
        ld      de,00080H
        ld      a,(YF306)
        or      a                       ; Random Block ?
        jr      nz,A48A8
        ld      a,(iy+14)
        ld      d,(iy+15)               ; yep, use user set recordsize
        ld      e,a
        or      d                       ; zero recordsize ?
        jr      nz,A48A8
        ld      e,128
        ld      (iy+14),e               ; yep, use 128 bytes default
A48A8:  inc     d
        dec     d
        jr      nz,A48B1
        ld      a,e
        cp      64
        jr      c,A48B5
A48B1:  xor     a
        ld      (YF2E4+3),a             ; recordsize >64, clear b31-b24 of record (use 24 bit recordnumbers)
A48B5:  ld      hl,(YF23D)
        ld      (YF2E2),hl              ; current transferaddress
        xor     a
        ld      (YF2DE),a               ; no error in recordoperation
        ld      (YF2DF),a               ; flag do not increase sector
        ld      bc,(YF2E8)              ; number of records requested
        call    A4916                   ; * recordsize
        ld      a,(iy+24)
        or      a
        ret     m                       ; DOS device, quit
        push    bc
        call    A4555                   ; get pointer to DPB of current drive
        ld      bc,(YF2E4+0)
        call    A4916                   ; multiply
        ld      (YF2F4+0),bc
        push    bc
        ld      bc,(YF2E4+2)
        call    A491C                   ; multiply high word
        ld      (YF2F4+2),bc            ; startbyte = startrecord * recordsize
        ld      h,b
        ld      l,c
        pop     bc                      ; BCHL = startbyte
        ld      e,(ix+2)
        ld      d,(ix+3)
        call    A4932                   ; / sectorsize
        ld      (YF2F2),hl              ; offset in sector of startbyte
        ld      (YF2EE),bc              ; relative sector of startbyte
        ld      a,(ix+6)
        and     c                       ; clustermask
        ld      (YF2DD),a               ; current relative sector in cluster (of startbyte)
        ld      a,(ix+7)                ; clustershift
A4906:  dec     a
        jr      z,A490F
        srl     b
        rr      c
        jr      A4906

A490F:  ld      (YF2EC),bc              ; relative cluster of startbyte
        pop     bc
        xor     a
        ret

;       Subroutine      multiply
;       Inputs          
;       Outputs         ________________________

A4916:  call    XF27C
        ld      hl,0

;       Subroutine      multiply high word
;       Inputs          
;       Outputs         ________________________

A491C:  ld      a,b
        ld      b,011H
        jr      A4928

A4921:  jr      nc,A4924
        add     hl,de
A4924:  rr      h
        rr      l
A4928:  rra
        rr      c
        djnz    A4921
        ld      b,a
        ret

;       Subroutine      divide
;       Inputs          
;       Outputs         ________________________

DIV16:
A492F:  ld      hl,0

;       Subroutine      divide
;       Inputs          
;       Outputs         ________________________

A4932:  call    XF27F
        ld      a,b
        ld      b,010H
        rl      c
        rla
A493B:  rl      l
        rl      h
        jr      c,A494E
        sbc     hl,de
        jr      nc,A4946
        add     hl,de
A4946:  ccf
A4947:  rl      c
        rla
        djnz    A493B
        ld      b,a
        ret

A494E:  or      a
        sbc     hl,de
        jr      A4947

;       Subroutine      calculate partial sector transfers
;       Inputs          
;       Outputs         ________________________

A4953:  ld      h,b
        ld      l,c                     ; bytes to transfer
        ld      bc,(YF2F2)              ; offset in sector startbyte
        ld      a,b
        or      c
        ld      e,a
        ld      d,a
        jr      z,A4972                 ; at start sector, no partial start
        ld      e,(ix+2)
        ld      d,(ix+3)
        ex      de,hl                   ; sectorsize
        sbc     hl,bc
        ex      de,hl                   ; bytes left in sector
        sbc     hl,de                   ; enough ?
        jr      nc,A4972                ; nop, get what you can
        add     hl,de
        ex      de,hl
        ld      hl,0
A4972:  ld      (YF2F8),de              ; bytes to transfer from partial sector
        ld      c,l
        ld      b,h                     ; bytes left after partial sector transfer
        ld      e,(ix+2)
        ld      d,(ix+3)
        call    A492F                   ; / sectorsize
        ld      (YF2FA),hl              ; partial bytes in endsector
        ld      (YF2FC),bc              ; hole sectors of transfer
        ret

;       Subroutine      get absolute cluster
;       Inputs          
;       Outputs         ________________________

A4989:  call    XF282
        ld      l,(iy+28)
        ld      h,(iy+29)               ; current cluster of file
        ld      e,(iy+30)
        ld      d,(iy+31)               ; current relative cluster of file
        ld      a,l
        or      h
        jr      z,A49CF                 ; file has no start cluster,
        push    bc
        ld      a,c
        sub     e
        ld      c,a
        ld      a,b
        sbc     a,d
        ld      b,a
        jr      nc,A49B0                ; requested cluster behind current, search from current cluster
        pop     bc
        ld      de,0                    ; relative cluster 0
        ld      l,(iy+26)
        ld      h,(iy+27)               ; start cluster of file
        push    af
A49B0:  pop     af
A49B1:  call    XF285
        ld      a,b
        or      c
        ret     z
        push    de
        push    hl
        call    A41F4                   ; get FAT entry content
        pop     de
        ld      a,h
        cp      00FH
        jr      c,A49C7
        ld      a,l
        cp      0F8H
        jr      nc,A49CC                ; end cluster
A49C7:  pop     de
        inc     de
        dec     bc
        jr      A49B1

A49CC:  ex      de,hl
        pop     de
        ret

A49CF:  inc     bc                      ; BC<>0 (means not found)
        dec     de
        ret

;       Subroutine      read datasector
;       Inputs          
;       Outputs         ________________________

A49D2:  ld      (YF2E0),a
        ld      hl,(YF2EC)              ; relative cluster
        ld      a,(YF2DD)               ; current relative sector in cluster
        call    A4EDB                   ; get sectornumber of cluster
        ex      de,hl
        ld      hl,(YF23F)
        sbc     hl,de                   ; is it currently in the datasector buffer ?
        jr      nz,A49F0                ; nope, get it
        ld      a,(YF2E1)               ; current driveid
        ld      l,a
        ld      a,(YF241)
        cp      l                       ; same drive as owner datasector buffer ?
        jr      z,A4A1B                 ; yep,
A49F0:  push    de
        push    ix
        call    A472D                   ; flush datasector buffer
        pop     ix
        pop     de
        ld      a,(YF2E0)
        or      a                       ; real or fake read ?
        jr      nz,A4A0D                ; fake read
        dec     a
        ld      (YF241),a
        ld      hl,(YF34F)              ; datasector buffer
        ld      b,1                     ; 1 sector
        push    de
        call    A46C5                   ; read sector with DOS error handling
        pop     de
A4A0D:  ld      (YF23F),de              ; current datasector
        ld      a,(YF2E1)               ; current driveid
        ld      (YF241),a               ; set owner datasector buffer
        ld      (YF243),ix              ; save DPB pointer
A4A1B:  ld      a,1
        ld      (YF2DF),a               ; flag do increase sector
        ld      hl,(YF2E2)
        push    hl
        ld      bc,(YF2F8)
        add     hl,bc
        ld      (YF2E2),hl              ; update current transferaddress
        ld      hl,(YF34F)              ; datasector buffer
        ld      de,(YF2F2)
        add     hl,de
        pop     de
        ret

;       Subroutine      do partical sector read if needed
;       Inputs          
;       Outputs         ________________________

A4A36:  call    XF288
        ld      hl,(YF2F8)
        ld      a,h
        or      l                       ; partial sector read
        ret     z                       ; nope, quit
        xor     a                       ; real read
        call    A49D2                   ; read datasector
        jp      XF1D9                   ; transfer to DOS memory

;       Subroutine      handle partial sector write
;       Inputs          
;       Outputs         ________________________

A4A46:  call    XF28B
        ld      hl,(YF2F8)
        ld      a,h
        or      l                       ; partial start ?
        ret     z                       ; nop, quit
        ld      hl,(YF2EE)
        inc     hl
        ld      (YF2EE),hl              ; update relative sector of startbyte
        xor     a
        ex      de,hl
        ld      hl,(YF2F0)
        sbc     hl,de                   ; sector behind end of file ?
        rra                             ; if yes, fake read
        call    A49D2                   ; read datasector
        ex      de,hl
        call    XF1D9                   ; transfer from DOS memory
        ld      a,1
        ld      (YF242),a               ; flag datasector changed
        ret

;       Subroutine      last partial sector ?
;       Inputs          
;       Outputs         ________________________

A4A6B:  ld      hl,0
        ld      (YF2F2),hl
        ld      hl,(YF2FA)
        ld      (YF2F8),hl
        ld      a,h
        or      l
        scf
        ret     z

;       Subroutine      to next sector (only when partical read was done)
;       Inputs          
;       Outputs         ________________________

A4A7B:  ld      a,(YF2DF)
        or      a                       ; flag do not increase
        ret     z                       ; yep, quit
        ld      a,(YF2DD)
        cp      (ix+6)                  ; clustermask
        jr      c,A4AA2                 ; still sectors left in this cluster, increase relative sector in cluster
        ld      de,(YF2EC)              ; current cluster of file
        ld      hl,00FF7H
        sbc     hl,de
        ret     c                       ; is the end cluster, quit
        ex      de,hl
        call    A41F4                   ; get FAT entry content
        ld      (YF2EC),hl              ; new current cluster of file
        ld      hl,(YF2EA)
        inc     hl
        ld      (YF2EA),hl              ; new current relative cluster of file
        ld      a,0FFH                  ; relative sector in cluster 0
A4AA2:  inc     a
        ld      (YF2DD),a
        or      a
        ret

; finish CON read

A4AA8:  ld      a,(hl)
        ldi
        cp      00DH
        jr      nz,A4AB1
        ld      (hl),00AH
A4AB1:  cp      00AH
        jr      z,A4ACA
        ld      a,b
        or      c
        jr      nz,A4AA8
A4AB9:  ld      (YF300),hl

; finish read record for dos devices

A4ABC:  ld      (YF2E2),de              ; update current transferaddress
        jp      nz,A4BE2
        res     6,(iy+24)
        jp      A4BE2

A4ACA:  call    A53A8                   ; console output
        ld      hl,00000H
        ld      a,c
        or      b
        jr      nz,A4AF9
        inc     a
        jr      A4AB9

;       Subroutine      read record for dos devices
;       Inputs          
;       Outputs         ________________________

A4AD7:  ld      de,(YF2E2)              ; current transferaddress
        inc     a
        jr      z,A4AF2                 ; CON, handle
        inc     a
        jr      nz,A4ABC                ; PRN, quit

; read record AUX

A4AE1:  call    A546E                   ; auxiliary input
        ld      (de),a
        inc     de
        cp      01AH
        jr      z,A4ABC                 ; CTRL-Z, 
        dec     bc
        ld      a,b
        or      c                       ; all bytes done ?
        jr      nz,A4AE1                ; nope, next byte
        inc     a
        jr      A4ABC

; read record CON

A4AF2:  ld      hl,(YF300)
        ld      a,h
        or      l
        jr      nz,A4AA8

; VERSION 1.1 CHANGE
; CHANGED, BUGFIX
; ld hl,128

A4AF9:  ld      hl,127
        ld      a,(YCONBF+0)
        cp      l
        jr      z,A4B05
        ld      (YCONBF+0),hl
A4B05:  push    bc
        push    de
        ld      de,YCONBF
        call    A50E0                   ; BDOS 0A (buffered console input)
        pop     de
        pop     bc
        ld      hl,YCONBF+2
        ld      a,(hl)
        cp      01AH
        jr      nz,A4AA8
        ld      (de),a
        inc     de
        ld      a,00AH                  ; LF
        call    A53A8                   ; console output
        xor     a
        ld      h,a
        ld      l,a
        jr      A4AB9

;       Subroutine      read record
;       Inputs          DEBC = recordnumber, HL = number of records
;       Outputs         DEHL = last record done, BC = number of records done

A4B23:  call    A487D                   ; initialize record info
        jp      m,A4AD7                 ; dos device, special action
        ld      l,(iy+16)
        ld      h,(iy+17)
        ld      de,(YF2F4+0)
        or      a
        sbc     hl,de
        push    hl
        ld      l,(iy+18)
        ld      h,(iy+19)
        ld      de,(YF2F4+2)
        sbc     hl,de
        pop     hl
        jp      c,A4C97                 ; startbyte behind end of file, quit with nothing done
        jr      nz,A4B56                ; startbyte at least 65536 bytes from the end of file, go get it
        ld      a,h
        or      l
        jp      z,A4C97                 ; startbyte is at end of file, quit with nothing done
        push    hl
        sbc     hl,bc                   ; requested number of bytes past file ?
        pop     hl
        jr      nc,A4B56                ; nope, go get it
        ld      b,h
        ld      c,l                     ; only read number of bytes until the end of file
A4B56:  call    XF28E
        call    A4953                   ; calculate partial sector transfers
        ld      bc,(YF2EC)              ; relative cluster
        call    A4989                   ; get absolute cluster
        ld      a,b
        or      c                       ; found ?
        jp      nz,A4C97                ; nope, quit with nothing done
        ld      (YF2EC),hl              ; current cluster = cluster of startbyte
        ld      (YF2EA),de              ; current relative cluster = relative cluster of startbyte
        call    A4A36                   ; do partical sector read if needed
        ld      hl,(YF2FC)
        ld      a,h
        or      l
        jp      z,A4BDC                 ; not any whole sectors to transfer, to partical end
        call    A4A7B                   ; to next sector (only when partical read was done)
        jr      c,A4BE2                 ; there is no next,
        ld      a,1
        ld      (YF2DF),a               ; flag do increase sector
        ld      a,(YF2DD)               ; current relative sector in cluster
        ld      bc,(YF2FC)
        ld      hl,(YF2EC)              ; current cluster of file
A4B8E:  push    bc
        call    A4E48                   ; calculate sequential sectors
        push    bc
        push    af
        ld      b,a
        call    A46C5                   ; read sectors with DOS error handling
        pop     af
        ld      c,a
        ld      b,000H                  ; number of sectors read
        jr      c,A4BC1                 ; sectors read does not include the sector in the datasector buffer
        ld      a,(YF242)
        or      a                       ; datasector buffer changed ?
        jr      z,A4BC1                 ; nope, then no need to transfer the datasector buffer
        push    bc
        ld      c,(ix+2)
        ld      b,(ix+3)                ; sectorsize
        push    bc
        push    hl
        ld      hl,(YF23F)              ; sectornumber of datasector buffer
        sbc     hl,de
        ex      de,hl
        call    A4916                   ; multiply
        pop     hl
        add     hl,bc
        pop     bc
        ex      de,hl
        ld      hl,(YF34F)              ; datasector buffer
        call    XF1D9                   ; transfer to DOS memory
        pop     bc
A4BC1:  pop     de
        pop     hl
        or      a
        sbc     hl,bc                   ; done all whole sectors ?
        jr      z,A4BDC                 ; yep, go partial end
        ld      c,l
        ld      b,h
        ld      hl,00FF7H
        sbc     hl,de                   ; end cluster ?
        jr      c,A4BE2                 ; yep, finish without partial end
        ld      hl,(YF2EA)
        inc     hl
        ld      (YF2EA),hl              ; increase current relarive cluster
        xor     a                       ; current relative sector in cluster = first sector
        ex      de,hl
        jr      A4B8E                   ; again

A4BDC:  call    A4A6B                   ; last partial sector ?
        call    nc,A4A36                ; yes, do partical sector read if needed
A4BE2:  call    XF291
        ld      hl,(YF2E2)              ; current transferaddress (end)
        ld      de,(YF23D)              ; transferaddress (begin)
        or      a
        sbc     hl,de
        ld      c,l
        ld      b,h                     ; size of transfer
        ld      de,00080H
        ld      a,(YF306)
        or      a                       ; Random Block
        jr      nz,A4C00                ; nope, use 128 bytes recordsize
        ld      e,(iy+14)
        ld      d,(iy+15)               ; user recordsize for Random Block
A4C00:  call    A492F                   ; how many records ?
        ld      a,h
        or      l                       ; partly records ?
        jr      z,A4C17                 ; nop,
        inc     bc                      ; records +1
        ex      de,hl
        sbc     hl,de                   ; 'missed' bytes
        ld      de,(YF2E2)
A4C0F:  xor     a
        ld      (de),a
        inc     de
        dec     hl
        ld      a,h
        or      l
        jr      nz,A4C0F                ; clear 'missed' bytes
A4C17:  ld      hl,(YF2E8)              ; number of records requested
        sbc     hl,bc
        jr      z,A4C22                 ; all done,
        inc     a
        ld      (YF2DE),a               ; error in record operation
A4C22:  call    XF294
        ld      hl,(YF2EC)
        ld      (iy+28),l
        ld      (iy+29),h               ; current cluster of file FCB
        ld      hl,(YF2EA)
        ld      (iy+30),l
        ld      (iy+31),h               ; current relative cluster of file FCB
A4C37:  ld      hl,(YF2E4+0)
        ld      de,(YF2E4+2)            ; startrecord
        ld      a,b
        or      c                       ; done any records ?
        ret     z                       ; nope, quit
        dec     bc
        add     hl,bc
        inc     bc
        ret     nc
        inc     de                      ; return current record
        ret

;       Subroutine      write record for dos devices
;       Inputs          
;       Outputs         ________________________

A4C47:  ld      hl,(YF23D)              ; transferaddress
        or      040H
        inc     a
        jr      z,A4C73                 ; CON, handle
        inc     a
        jr      z,A4C63                 ; AUX, handle
        inc     a
A4C53:  jr      z,A4C81                 ; NUL, handle

        ld      a,(hl)
        inc     hl
        cp      01AH
        jr      z,A4C81
        call    A5466                   ; printer output
        dec     bc
        ld      a,b
        or      c
        jr      A4C53

A4C63:  ld      a,(hl)
        inc     hl
        call    A5475                   ; auxiliary output
        cp      01AH
        jr      z,A4C81
        dec     bc
        ld      a,b
        or      c
        jr      nz,A4C63
        jr      A4C81

A4C73:  ld      a,(hl)
        inc     hl
        cp      01AH
        jr      z,A4C81
        call    A53A8                   ; console output
        dec     bc
        ld      a,b
        or      c
        jr      nz,A4C73
A4C81:  ld      bc,(YF2E8)              ; no. of records
        jr      A4C37

A4C87:  ld      c,e
        ld      b,d                     ; clusters to skip
        call    A49B1                   ; get next absolute cluster
        ld      a,b
        or      c                       ; found ?
        jp      z,A4D41                 ; yep,
        call    A4F12                   ; allocate cluster chain
        jp      nc,A4D41                ; ok, go writing

A4C97:  call    XF297
        xor     a
        ld      c,a
        ld      b,a                     ; no records read/write
        inc     a
        ld      (YF2DE),a               ; error in record operation
        jr      A4C37

;       Subroutine      write record
;       Inputs          DEBC = recordnumber, HL = number of records
;       Outputs         ________________________

A4CA3:  call    A487D                   ; initialize record info
        push    af
        push    bc
        call    A5496                   ; get time and date (dirformat)
        ld      (iy+20),c
        ld      (iy+21),b
        ld      (iy+22),e
        ld      (iy+23),d
        pop     bc
        pop     af
        jp      m,A4C47                 ; DOS device, special action
        res     6,(iy+24)               ; flag FCB changed
        push    bc
        call    A4953                   ; calculate partical sector transfers
        pop     bc
        ld      hl,(YF2F4+0)
        ld      de,(YF2F4+2)            ; startbyte
        ld      a,b
        or      c                       ; zero bytes to write (only possible with Random Block) ?
        jp      z,A4DDD                 ; yep, filesize adjust action
        dec     bc
        add     hl,bc
        jr      nc,A4CD6
        inc     de                      ; endbyte
A4CD6:  ld      b,h
        ld      c,l
        ex      de,hl
        ld      e,(ix+2)
        ld      d,(ix+3)
        call    A4932                   ; / sectorsize
        ld      h,b
        ld      l,c                     ; relative sector of endbyte
        ld      b,(ix+7)                ; clustershift
        dec     b
        jr      z,A4CF0
A4CEA:  srl     h
        rr      l
        djnz    A4CEA                   ; relative cluster of endbyte
A4CF0:  push    hl
        ld      c,(iy+16)
        ld      b,(iy+17)
        ld      l,(iy+18)
        ld      h,(iy+19)               ; filesize
        call    A4932                   ; / sectorsize
        ld      a,h
        or      l                       ; offset in sector
        jr      z,A4D05
        inc     bc                      ; relative sector
A4D05:  call    XF29A
        ld      (YF2F0),bc              ; relative sector behind fileend
        ld      bc,(YF2EC)              ; relative cluster of startbyte
        call    A4989                   ; get absolute cluster
        ld      (YF2EC),hl              ; current cluster = cluster of startbyte
        ld      (YF2EA),de              ; current relative cluster = relative cluster of startbyte
        ex      (sp),hl
        or      a
        sbc     hl,de                   ; start and endbyte in same cluster ?
        ex      de,hl
        pop     hl
        jr      z,A4D41                 ; yep,
        ld      a,b
        or      c                       ; is cluster of startbyte found ?
        jp      z,A4C87                 ; yep, make chain to cluster of endbyte if needed and start writing
        push    bc
        ld      c,e
        ld      b,d                     ; clusters to allocate
        call    A4F12                   ; allocate cluster chain
        pop     bc
        jp      c,A4C97                 ; failed, quit with nothing done
        ld      de,(YF2EA)
        inc     de                      ; relative cluster to start
        dec     bc                      ; clusters to skip
        call    A49B1                   ; get next absolute cluster
        ld      (YF2EC),hl              ; cluster of startbyte
        ld      (YF2EA),de              ; relative cluster of startbyte
A4D41:  call    A4A46                   ; handle partial sector write
        ld      hl,(YF2FC)
        ld      a,h
        or      l                       ; any complete sectors ?
        jr      z,A4D8C                 ; nope, goto partial end
        ld      de,(YF2EE)
        add     hl,de
        ld      (YF2EE),hl              ; update relative sector of startbyte
        call    A4A7B                   ; to the next sector (only when partial write was done)
        ld      a,1
        ld      (YF2DF),a               ; flag do increase sector
        ld      a,(YF2DD)               ; current relative sector in cluster
        ld      hl,(YF2EC)              ; relative cluster
        ld      bc,(YF2FC)              ; whole sectors
A4D65:  push    bc
        call    A4E48                   ; calculate sequencial sectors
        push    bc
        push    af
        ld      b,a
        jr      c,A4D73                 ; sectors writen does not include the sector in the datasector buffer
        ld      a,0FFH
        ld      (YF241),a               ; invalid datasector buffer
A4D73:  call    A4755                   ; write datasectors with DOS error handling
        pop     af
        pop     de
        pop     hl
        ld      c,a
        xor     a
        ld      b,a
        sbc     hl,bc                   ; whole sectors left ?
        jr      z,A4D8C                 ; nop, go to partial end
        ld      c,l
        ld      b,h
        ld      hl,(YF2EA)
        inc     hl
        ld      (YF2EA),hl              ; update relative cluster
        ex      de,hl
        jr      A4D65                   ; again

A4D8C:  call    XF29D
        call    A4A6B                   ; last partial sector ?
        call    nc,A4A46                ; partial end, handle partial sector write
        ld      hl,(YF2E2)              ; current transferaddress
        ld      de,(YF23D)              ; transferaddress
        or      a
        sbc     hl,de
        ld      de,(YF2F4+0)
        add     hl,de
        ld      de,(YF2F4+2)
        jr      nc,A4DAB
        inc     de
A4DAB:  ld      (YF2F4+0),hl
        ld      (YF2F4+2),de            ; startbyte = startbyte + transfersize
        ld      c,(iy+16)
        ld      b,(iy+17)
        or      a
        sbc     hl,bc
        ld      c,(iy+18)
        ld      b,(iy+19)
        ex      de,hl
        sbc     hl,bc                   ; has file expanded ?
        jr      c,A4DD6                 ; nop,
A4DC6:  push    iy
        pop     hl
        ld      de,00010H
        add     hl,de
        ex      de,hl
        ld      hl,YF2F4                ; filelength = endbyte
        ld      bc,4
        ldir
A4DD6:  ld      bc,(YF2E8)              ; no. of records
        jp      A4C22

; filesize adjust

A4DDD:  ld      a,h
        or      l
        or      d
        or      e                       ; startbyte zero ?
        jr      z,A4E32                 ; yep, kill chain and quit
        ld      bc,1
        sbc     hl,bc
        ex      de,hl
        dec     bc
        sbc     hl,bc
        ld      b,d
        ld      c,e                     ; filesize = startbyte-1
        ld      e,(ix+2)
        ld      d,(ix+3)
        call    A4932                   ; / sectorsize
        ld      a,(ix+7)                ; clustershift
A4DFA:  dec     a
        jr      z,A4E03
        srl     b
        rr      c
        jr      A4DFA

A4E03:  call    A4989                   ; get absolute cluster
        ld      a,b
        or      c                       ; found ?
        jr      z,A4E26                 ; yep, this means chain must be shortend
        call    A4F12                   ; allocate cluster chain
        jp      c,A4C97                 ; failed, quit with nothing done
A4E10:  ld      bc,0
        ld      (YF2E8),bc              ; number of records = 0
        ld      (YF2EA),bc              ; current relative cluster = 0
        ld      l,(iy+26)
        ld      h,(iy+27)               ; start cluster of file
        ld      (YF2EC),hl              ; current cluster = start cluster of file
        jr      A4DC6

A4E26:  ld      bc,00FFFH
        call    A4F9E                   ; mark end & release rest chain
A4E2C:  dec     de

; VERSION 1.1 CHANGE
; CHANGED, BUGFIX
; ld a,0FFH

        ld      a,1
        ld      (de),a                  ; flag FAT buffer changed
        jr      A4E10

A4E32:  ld      l,(iy+26)
        ld      h,(iy+27)
        ld      a,h
        or      l                       ; file has start cluster ?
        jr      z,A4E10                 ; nop,
        xor     a
        ld      (iy+26),a
        ld      (iy+27),a               ; file has no start cluster (empty file)
        call    A4F9B                   ; release cluster chain
        jr      A4E2C                   ; mark FAT buffer changed

;       Subroutine      calculate sequencial sectors
;       Inputs          
;       Outputs         ________________________

A4E48:  call    XF2A0
        ld      d,a
        push    hl
        inc     b
        dec     b
        jr      z,A4E53
        ld      c,0FFH
A4E53:  ld      e,c
        push    de
        ld      a,(ix+6)                ; clustermask
        ld      (YF2DD),a               ; current relative sector in cluster
        inc     a
        sub     d
        ld      b,a
A4E5E:  ld      (YF2EC),hl
        push    hl
        call    A41F4                   ; get FAT entry content
        pop     de
        ld      a,c
        sub     b
        ld      c,a
        jr      z,A4E78
        ld      b,(ix+6)                ; clustermask
        jr      c,A4ECA
        inc     b
        inc     de
        ex      de,hl
        sbc     hl,de
        ex      de,hl
        jr      z,A4E5E
A4E78:  pop     de
        ex      (sp),hl
        push    hl
        push    de
        ld      a,e
        sub     c
        ld      e,a
        ld      d,000H
        ld      c,(ix+2)
        ld      b,(ix+3)                ; sectorsize
        call    A4916                   ; multiply
        pop     af
        ld      hl,(YF2E2)
        push    hl
        add     hl,bc
        ld      (YF2E2),hl              ; update current transferaddress
        pop     bc
        pop     hl
        push    bc
        push    de
        ex      de,hl
        ld      hl,(YF2EC)
        sbc     hl,de
        ld      bc,(YF2EA)
        add     hl,bc
        ld      (YF2EA),hl
        ex      de,hl
        call    A4EDB                   ; get sectornumber of cluster
        ex      de,hl
        pop     bc
        ld      a,(YF241)
        cp      (ix+0)                  ; driveid
        ld      a,c
        scf
        jr      nz,A4EC7
        ld      hl,(YF23F)              ; sectornumber of datasector buffer
        or      a
        sbc     hl,de
        jr      c,A4EC7
        ld      h,b
        ld      l,c
        add     hl,de
        dec     hl
        ld      bc,(YF23F)              ; sectornumber of datasector buffer
        sbc     hl,bc
A4EC7:  pop     hl
        pop     bc
        ret

A4ECA:  add     a,b
        ld      (YF2DD),a               ; current relative sector in cluster
        ld      c,000H
        jr      A4E78

;       Subroutine      get decoded characterpair (not needed, uses for secret message)
;       Inputs          
;       Outputs         ________________________

A4ED2:  call    A41FA
        ld      a,l
        add     hl,hl
        add     hl,hl                   ; second char in H
        and     03FH                    ; first char in A
        ret

;       Subroutine      get sectornumber of cluster
;       Inputs          HL = cluster, A = relative sector in cluster
;       Outputs         HL = sectornumber

A4EDB:  call    XF2A3
        push    bc
        ld      b,(ix+7)                ; clustershift
        dec     hl
        dec     hl
        dec     b
        jr      z,A4EED
A4EE7:  sla     l
        rl      h
        djnz    A4EE7
A4EED:  or      l
        ld      l,a
        ld      c,(ix+12)
        ld      b,(ix+13)
        add     hl,bc                   ; + first datasector
        pop     bc
        ret

;       Subroutine      get recordnumber from S2,EX and CR fields
;       Inputs          
;       Outputs         ________________________

A4EF8:  push    de
        pop     iy
        ld      c,(iy+32)               ; CR (current record)
        ld      b,(iy+12)               ; EX (extent)
        ld      e,(iy+14)               ; S2
        ld      d,0
        sla     c
        srl     e
        rr      b
        rr      c                       ; debc = recordnumber
        ld      hl,1                    ; 1 record
        ret

;       Subroutine      allocate cluster chain
;       Inputs          
;       Outputs         ________________________

A4F12:  call    XF2A6
        ld      e,(ix+19)
        ld      d,(ix+20)
        ex      de,hl                   ; pointer to FAT buffer of drive
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a                     ; pointer to FAT
        push    hl
        ex      de,hl
        ld      e,(ix+14)
        ld      d,(ix+15)               ; Max cluster
        ld      (YF302),de              ; store
        push    hl
A4F2D:  push    bc                      ; clusters to allocate
        push    hl                      ; start cluster
        ld      d,h
        ld      e,l
A4F31:  push    de
        ex      de,hl
        ld      hl,(YF302)
        dec     hl
        or      a
        sbc     hl,de
        ex      de,hl                   ; last cluster on disk ?
        pop     de
        jr      nc,A4F4E                ; nop, go up
        ld      a,e
        or      d                       ; search below finished ?
        jr      nz,A4F56                ; nop, go below
        pop     hl
        pop     hl
        pop     hl
        ld      bc,00FFFH
        call    A4F9E                   ; mark end & release rest chain
        scf
        jr      A4F94

A4F4E:  inc     hl
        call    A4F5E                   ; try to allocate
        ld      a,e                     ; nop not free !
        or      d                       ; search below finished ?
        jr      z,A4F31                 ; try again (up)
A4F56:  dec     de
        ex      de,hl
        call    A4F5E                   ; try to allocate
        ex      de,hl                   ; nop not free !
        jr      A4F31                   ; try again

;       Subroutine      allocate cluster if free
;       Inputs          
;       Outputs         ________________________

A4F5E:  push    hl
        push    de
        call    A41F4                   ; cluster free ?
        pop     de
        pop     hl
        ret     nz                      ; nop, no alloc
        pop     bc
        ld      c,l
        ld      b,h
        ex      (sp),hl
        ld      e,(ix+19)
        ld      d,(ix+20)               ; pointer to FAT buffer of drive
        call    A4221                   ; set FAT entry content
        pop     hl
        pop     bc
        dec     bc
        ld      a,b
        or      c
        jr      nz,A4F2D
        ld      bc,00FFFH               ; chain endmarker
        call    A4221                   ; set FAT entry content
        dec     de
        ld      a,1
        ld      (de),a                  ; FAT changed
        pop     hl
        push    hl
        call    A41F4                   ; get FAT entry content
        pop     bc
        ld      a,c
        or      b
        jr      nz,A4F94
        ld      (iy+26),l
        ld      (iy+27),h               ; start cluster of file
A4F94:  ex      de,hl
        pop     bc
        ld      (hl),c
        inc     hl
        ld      (hl),b
        ex      de,hl
        ret

;       Subroutine      release cluster chain
;       Inputs          
;       Outputs         ________________________

A4F9B:  ld      bc,0

;       Subroutine      set cluster entry and release rest of cluster chain
;       Inputs          
;       Outputs         ________________________

A4F9E:  call    XF2A9
        push    hl
        call    A41F4                   ; get FAT entry content
        ex      (sp),hl
        call    A4221                   ; set FAT entry content
        pop     hl
        ld      a,h
        or      l
        ret     z
        ld      a,h
        cp      00FH
        jr      c,A4F9B
        ld      a,l
        cp      0F8H
        jr      c,A4F9B                 ; not end of chain, release
        ret

;       Subroutine      BDOS 11 (search for first)
;       Inputs          
;       Outputs         ________________________

A4FB8:  call    A42A5                   ; validate FCB, clear S2 and find direntry
A4FBB:  jr      c,A5000                 ; error, quit
        ld      a,(YF2B8)
        jr      z,A4FC4                 ; file, save direntry number for search next
        ld      a,0FFH                  ; device, flag search next invalid
A4FC4:  ld      (YF30B),a
        ld      (YF309),ix              ; save pointer to DPB
        ld      de,(YF23D)              ; transferaddress
        ld      a,(YF2E1)               ; current driveid
        inc     a
        ld      (de),a
        inc     de
        ld      a,(hl)
        cp      005H
        jr      nz,A4FDC
        ld      (hl),0E5H
A4FDC:  ld      bc,32
        call    XF1D9                   ; transfer direntry to DOS memory (?? LDIR is also sufficient)
        call    A4439                   ; get max record and extent
        ld      a,(YF30C)
        cp      b                       ; orginal FCB EX byte same as max extent ?
        jr      z,A4FEF                 ; same, RC = max record
        jr      nc,A5000                ; bigger, quit with error
        ld      c,080H                  ; smaller, RC = 128 (means extend is full)
A4FEF:  ld      hl,(YF23D)              ; transferaddress
        ld      de,0000CH
        add     hl,de
        ld      b,(hl)                  ; MS-DOS fileattribute
        ld      (hl),a                  ; EX = orginal FCB EX byte (CP/M: requested extent)
        inc     hl
        ld      (hl),b                  ; S1 = MS-DOS fileattribute (CP/M: reserved)
        inc     hl
        ld      (hl),d                  ; S2 = 0 (CP/M: extent high byte)
        inc     hl
        ld      (hl),c                  ; RC = (CP/M: recordcount)
        xor     a                       ; CP/M direntry 0, no error
        ret

A5000:  ld      a,0FFH
        ld      (YF30B),a               ; search for next invalid
        ret

;       Subroutine      BDOS 12 (search for next)
;       Inputs          
;       Outputs         ________________________

A5006:  call    A440E                   ; validate FCB drive and filename
        jr      c,A5000                 ; invalid,
        ld      a,(YF30B)               ; saved direntrynumber of last search first
        cp      0FFH
        jr      z,A5000                 ; flag search next invalid, quit with error
        ld      (YF2B8),a
        ld      ix,(YF309)              ; saved pointer to DPB
        call    A42BC                   ; find next directoryentry
        jr      A4FBB                   ; finish

;       Subroutine      BDOS 23 (compute filesize)
;       Inputs          
;       Outputs         ________________________

A501E:  call    A42A5                   ; validate FCB, clear S2 and find direntry
        ld      a,0FFH
        ret     c                       ; error, quit
        push    de
        pop     ix
        ld      a,(iy+28)
        ld      c,(iy+29)
        ld      b,(iy+30)
        ld      e,(iy+31)
        add     a,a
        rl      c
        rl      b
        rl      e                       ; convert filesize to random record
        or      a                       ; filesize a multiply of 128 ?
        jr      z,A5043
        inc     bc
        ld      a,b
        or      c
        jr      nz,A5043
        inc     e                       ; nope, increase random record
A5043:  ld      (ix+33),c
        ld      (ix+34),b
        ld      (ix+35),e               ; set R2,R1 and R0
        xor     a
        ret                             ; quit without error

;       Subroutine      BDOS 18 (return bitmap of logged-in drives)
;       Inputs          
;       Outputs         ________________________

A504E:  ld      a,(YF347)
        ld      b,a
        xor     a
A5053:  scf
        rla
        djnz    A5053                   ; all drives all online
        ret

;       Subroutine      BDOS 1A (set DMA address)
;       Inputs          
;       Outputs         ________________________

A5058:  ld      (YF23D),de              ; set transferaddress
        ret

;       Subroutine      BDOS 1B (MSXDOS get allocation)
;       Inputs          
;       Outputs         ________________________

A505D:  xor     a
        ld      (YF306),a               ; no CP/M call
        ld      a,e
        call    A4427                   ; validate fcb driveid
        ld      a,0FFH
        ret     c                       ; error, quit
        call    A44DB                   ; get latest FAT
        ld      e,(ix+19)
        ld      d,(ix+20)
        push    de
        pop     iy                      ; pointer to FAT buffer of drive
        ld      hl,2                    ; start at clusterentry 2
        ld      b,h
        ld      c,h                     ; free cluster = 0
        ld      e,(ix+14)
        ld      d,(ix+15)
        dec     de                      ; number of clusters on disk
        push    de
A5081:  push    de
        push    hl
        call    A41F4                   ; get FAT entry content
        pop     hl
        pop     de
        jr      nz,A508B
        inc     bc                      ; free clusters + 1
A508B:  inc     hl
        dec     de
        ld      a,e
        or      d
        jr      nz,A5081                ; next cluster
        ld      h,b
        ld      l,c                     ; number of free clusters
        pop     de                      ; number of clusters
        ld      a,(ix+6)
        inc     a                       ; number of sectors per cluster
        ld      c,(ix+2)
        ld      b,(ix+3)                ; sectorsize
        ret

;       Subroutine      BDOS 0D (reset discs)
;       Inputs          
;       Outputs         ________________________

A509F:  ld      hl,00080H
        ld      (YF23D),hl              ; default transferaddress
        xor     a
        ld      (YF247),a               ; default driveid 0 (A:)
        call    A472D                   ; flush datasector buffer
        ld      hl,YF355
        ld      a,(YF347)               ; all drives
A50B2:  ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to DPB
        inc     hl
        push    hl
        push    af
        push    de
        pop     ix
        call    A45C4                   ; flush FAT buffer
        pop     af
        pop     hl
        dec     a
        jr      nz,A50B2                ; next drive
        ret

;       Subroutine      BDOS 25 (return current drive)
;       Inputs          
;       Outputs         ________________________

A50C4:  ld      a,(YF247)
        ret

;       Subroutine      BDOS 34 (update random access pointer)
;       Inputs          
;       Outputs         ________________________

A50C8:  call    A4EF8                   ; get recordnumber from CR,EX and S2 field
        ld      (iy+33),l
        ld      (iy+34),h
        ld      (iy+35),e
        ret

;       Subroutine      BDOS 0E (select disc)
;       Inputs          
;       Outputs         ________________________

A50D5:  ld      a,(YF347)
        cp      e
        ret     c
        ret     z
        ld      hl,YF247
        ld      (hl),e
        ret

;       Subroutine      BDOS 0A (buffered console input)
;       Inputs          
;       Outputs         ________________________

A50E0:  push    de
        ld      a,(YF237)
        ld      (YF238),a               ; save current console columnpos to record start of inputline
        xor     a
        ld      (YF239),a               ; not in insertmode
        ld      h,d
        ld      l,e
        ld      b,a
        ld      c,(hl)                  ; size of buffer
        inc     hl
        ld      d,a
        ld      e,(hl)                  ; length of line already in buffer
        inc     hl
        ld      ix,YCONTP
        ld      a,e
        cp      c                       ; is lengthbyte valid ?

; VERSION 1.1 CHANGE
; CHANGED, BUGFIX
; jr nc,A5103

        jr      nc,A5101                ; equal, use the line in buffer as basis otherwise use empty line as basis
        push    hl                      ; length smaller than size of buffer
        add     hl,de
        ld      a,(hl)
        pop     hl
        cp      00DH                    ; then line must be terminated by a CR
A5101:  jr      z,A5104                 ; it is, use the line in buffer as basis
A5103:  ld      e,d                     ; use empty line as basis

; linputinput headloop, also lineinput CTRL-F

A5104:  call    XF2AC                   ; hook
        call    A544E                   ; BDOS 8 (direct input)
A510A:  push    hl
        push    bc
        ld      hl,T5374
        ld      bc,NKEYNT               ; number of keyentries
        cpir
        add     hl,bc
        add     hl,bc
        add     hl,bc
        ld      c,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,c
        pop     bc
        ex      (sp),hl
        ret

; lineinput CTRL-A, MSX graphic header

A511E:  call    A544E                   ; BDOS 8 (direct input)
        cp      040H
        jr      c,A510A
        cp      060H
        jr      nc,A510A
        push    af
        ld      a,b
        inc     a
        cp      c
        jr      nc,A515E                ; beep
        ld      a,1
        ld      (ix+0),a
        inc     ix
        inc     b
        call    A535D
        pop     af


; lineinput, normal key action

A513B:  push    af
        ld      a,b
        cp      c
        jr      nc,A515E                ; beep
        pop     af
        ld      (ix+0),a
        inc     ix
        inc     b
        call    A535D
        ld      a,(YF239)
        or      a                       ; insertmode ?
        jr      nz,A5104                ; yep,
        inc     d
        ld      a,e
        cp      d
        jr      c,A5104
        ld      a,(hl)
        dec     a
        inc     hl
        jr      nz,A5104
        inc     d
        inc     hl
        jr      A5104

A515E:  pop     af
        ld      a,007H                  ; bell
        call    A53A8                   ; console output
        jr      A5104

; lineinput UP key, ESC key, CTRL-U (VOID)

A5166:  pop     de
        ld      a,(YF238)
        ld      b,a
        ld      a,(YF237)
        sub     b                       ; length of the inputline
        jr      z,A5177                 ; empty inputline, restart line input
        ld      b,a
A5172:  call    A534F
        djnz    A5172
A5177:  jp      A50E0                   ; restart lineinput

; lineinput CTRL-J

A517A:  ld      a,b
        ld      (YF23A),a               ; store current linelength (for secret message)
        call    A5183                   ; newline
A5181:  jr      A5104

A5183:  ld      a,00DH
        call    A53A8                   ; console output
        ld      a,00AH
        jp      A53A8                   ; console output

; lineinput CR key

A518D:  pop     de
A518E:  call    A53A8                   ; console output
        push    de
        inc     de
        ld      a,b
        ld      (de),a
        cp      c
        push    af
        inc     de
        ld      c,b
        xor     a
        ld      b,a
        or      c
        jr      z,A51A3
        ld      hl,YCONTP
        ldir
A51A3:  pop     af
        jr      z,A51A9
        ld      a,00DH
        ld      (de),a
A51A9:  pop     de
        ret

; lineinput LEFT key, BS key (BS)

A51AB:  ld      a,(YF23A)
        and     b
        cp      07FH
        jp      z,A5244                 ; secret programmers message
        inc     b
        dec     b
        jr      z,A51DC
        dec     b
        dec     ix
        call    A534F
        inc     b
        dec     b
        jr      z,A51CE
        dec     b
        dec     ix
        ld      a,(ix+0)
        dec     a
        jr      z,A51DC
        inc     b
        inc     ix
A51CE:  ld      a,(ix+0)
        cp      020H
        jr      nc,A51DC
        cp      009H
        jr      z,A51FB
        call    A534F
A51DC:  ld      a,(YF239)
        or      a                       ; insertmode ?
        jr      nz,A5181                ; yep,
        inc     d
        dec     d
        jr      z,A5181
        dec     d
        ld      a,d
        cp      e
        jr      nc,A5181
        dec     hl
        ld      a,d
        cp      001H
        jr      c,A5181
        dec     hl
        ld      a,(hl)
        dec     a
        inc     hl
        jr      nz,A5181
        dec     d
        dec     hl
        jr      A5181

A51FB:  push    hl
        push    bc
        ld      a,(YF238)
        ld      c,a                     ; start of the inputline
        inc     b
        dec     b
        jr      z,A521A
        ld      hl,YCONTP
A5208:  ld      a,(hl)
        inc     hl
        cp      001H
        jr      z,A5218
        inc     c
        cp      020H
        jr      nc,A5218
        cp      009H
        jr      z,A522A
        inc     c
A5218:  djnz    A5208
A521A:  ld      a,(YF237)               ; current console columnpos
        sub     c
        jr      z,A5226
        ld      b,a
A5221:  call    A534F
        djnz    A5221
A5226:  pop     bc
        pop     hl
        jr      A51DC

A522A:  ld      a,c
        add     a,007H
        and     0F8H
        ld      c,a
        jr      A5218

; lineinput INS key (INSERT)

A5232:  ld      a,(YF239)
        xor     001H
        jr      A523E                   ; toggle insertmode

; unused code

        xor     a
        jr      A523E

; unused code

        ld      a,001H

A523E:  ld      (YF239),a
        jp      A5104

;       Subroutine      display message of programmer (not needed)
;       Inputs          
;       Outputs
;       Remark          activated by:
;                       input 127 or 255 chars, press CTRL-J, press BS or LEFT

A5244:  xor     a
        ld      (YF23A),a
        push    bc
        ld      b,16
        ld      de,T547D
        ld      hl,0
A5251:  push    hl
        call    A4ED2                   ; get decoded characterpair
        add     a,020H
        call    A53A8                   ; console output
        ld      a,h
        add     a,020H
        call    A53A8                   ; console output
        pop     hl
        inc     hl
        djnz    A5251
        pop     bc

; lineinput HOME key (NEWLINE)

A5265:  ld      a,040H
        pop     de
        call    A518E
        call    A5183                   ; newline
        ld      a,(YF238)
        or      a                       ; start of the inputline at the begin of a line ?
        jp      z,A50E0                 ; yep, restart lineinput routine
        ld      b,a
        ld      a," "
A5278:  call    A53A8                   ; console output
        djnz    A5278
        jp      A50E0                   ; restart lineinput routine

; lineinput DOWN key (COPYALL)

A5280:  ld      a,0FFH
        jr      A52B5

; lineinput CTRL-L (SKIPUP)

A5284:  call    A52E3
        jp      c,A5104
        push    bc
        ld      c,a
        ld      b,000H
        add     hl,bc
        pop     bc
        add     a,d
        ld      d,a
        jp      A5104

; lineinput SELECT key (COPYUP)

A5295:  call    A52E3
        jp      c,A5104
        jr      A52B5

; lineinput DEL key (SKIP1)

A529D:  ld      a,d
        cp      e
        jp      nc,A5104
        inc     d
        ld      a,(hl)
        dec     a
        inc     hl
        jp      nz,A5104
        inc     d
        inc     hl
        jp      A5104

; lineinput RIGHT key (COPY1)

A52AE:  ld      a,(hl)
        dec     a
        ld      a,001H
        jr      nz,A52B5
        inc     a
A52B5:  push    af
        xor     a
        ld      (YF239),a               ; insertmode off
        ld      a,b
        cp      c
        jr      nc,A52DF
        ld      a,d
        cp      e
        jr      nc,A52DF
        ld      a,(hl)
        cp      001H
        jr      nz,A52CD
        ld      a,b
        inc     a
        cp      c
        jr      nc,A52DF
        ld      a,(hl)
A52CD:  inc     hl
        ld      (ix+0),a
        inc     ix
        call    A535D
        inc     b
        inc     d
        pop     af
        dec     a
        jr      nz,A52B5
        jp      A5104

A52DF:  pop     af
        jp      A5104

A52E3:  call    A544E                   ; BDOS 8 (direct input)
        cp      001H
        jr      nz,A531F
        call    A544E                   ; BDOS 8 (direct input)
        cp      040H
        jr      c,A531F
        cp      060H
        jr      nc,A531F
        push    hl
        push    de
        push    bc
        ld      iy,00000H
A52FC:  scf
        push    af
        ld      a,001H
        call    A531F
        jr      c,A531A
        ld      c,a
        ld      b,000H
        add     hl,bc
        add     a,d
        ld      d,a
        push    iy
        pop     af
        add     a,c
        push    af
        pop     iy
        inc     hl
        pop     af
        cp      (hl)
        dec     hl
        jr      nz,A52FC
        push    iy
A531A:  pop     af
        pop     bc
        pop     de
        pop     hl
        ret

A531F:  push    bc
        push    af
        ld      a,e
        sub     d
        jr      c,A534B
        jr      z,A534B
        dec     a
        jr      z,A534B
        ld      c,a
        ld      b,000H
        pop     af
        push    hl
        push    af
        ld      a,(hl)
        dec     a
        jr      nz,A5336
        inc     hl
        dec     c
A5336:  pop     af
        inc     c
        dec     c
        jr      nz,A533F
        pop     hl
        pop     bc
        scf
        ret

A533F:  inc     hl
        cpir
        pop     hl
        jr      nz,A534C
        ld      a,e
        sub     d
        dec     a
        sub     c
        pop     bc
        ret

A534B:  pop     af
A534C:  pop     bc
        scf
        ret

A534F:  ld      a,008H
        call    A53A8                   ; console output
        ld      a,020H
        call    A53A8                   ; console output
        ld      a,008H
        jr      A53A8                   ; console output

A535D:  cp      020H
        jr      nc,A53A8                ; console output
        cp      009H
        jr      z,A53A8                 ; console output
        cp      001H
        jr      z,A53A8                 ; console output
        push    af
        ld      a,"^"
        call    A53A8                   ; console output
        pop     af
        or      040H
        jr      A53A8                   ; console output

; keytable lineinput
; first table contains all keycodes, code 8 at the end is a fake one for 'other key', because it is already in the table
; second table contains all serviceroutines, but in reserve order (so last one belongs to the first keycode)

T5374:  defb    006H,07FH,008H,00DH,00AH,015H,00BH,00CH
        defb    01BH,012H,018H,01CH,01DH,01EH,01FH,001H
        defb    008H

        defw    A513B
        defw    A511E,A5280,A5166,A51AB,A52AE,A5295,A5232,A5166
        defw    A5284,A5265,A5166,A517A,A518D,A51AB,A529D,A5104

NKEYNT  equ     ($-T5374)/3


;       Subroutine      BDOS 02 (console output)
;       Inputs          
;       Outputs         ________________________

A53A7:  ld      a,e
A53A8:  call    XF2AF
        cp      00BH
        jr      z,A53E8
        cp      00CH
        jr      z,A53E8
        cp      01CH
        jr      z,A53D5
        cp      01DH
        jr      z,A53F0
        cp      00DH
        jr      z,A53E8
        cp      008H
        jr      z,A53F0
        cp      009H
        jr      z,A53F8
        cp      07FH
        jr      z,A53F0
        cp      020H
        jr      c,A53D5
        push    hl
        ld      hl,YF237
        inc     (hl)                    ; increase console columnpos
        pop     hl
A53D5:  push    bc
        ld      b,a
        call    A408F                   ; output to screen
        nop
        call    A5412
        ld      a,(YF23B)
        or      a                       ; console output also to printer ?
        ld      a,b
        pop     bc
        ret     z                       ; nope, quit
        jp      A409B                   ; output to printer

A53E8:  push    af
        xor     a
        ld      (YF237),a               ; console columpos
        pop     af
        jr      A53D5

A53F0:  push    hl
        ld      hl,YF237
        dec     (hl)                    ; decrease console columnpos
        pop     hl
        jr      A53D5

A53F8:  ld      a," "
        call    A53A8                   ; console output
        ld      a,(YF237)
        and     007H
        jr      nz,A53F8                ; to the next console tabposition
        ret

A5405:  cp      010H                    ; CTRL-P ?
        jr      z,A541D                 ; yep, handle it
        cp      00EH                    ; CTRL-N ?
        jr      z,A541D                 ; yep, handle it
        cp      003H                    ; CTRL-C ?
        jr      z,A541D                 ; yep, handle it
        ret                             ; nope, quit

A5412:  call    A4034                   ; check if keyboardinput available
        ret     z                       ; nope, quit
        cp      013H                    ; CTRL-S ?
        jr      nz,A5405                ; nope, check other specials
        call    A4078                   ; get keyboardinput (the CTRL-S)
                                        ; next wait for other consoleinput
A541D:  call    A4078                   ; get keyboardinput
        cp      010H
        jr      z,A5431                 ; CTRL-P, enable printer output
        cp      00EH
        jr      z,A5437                 ; CTRL-N, disable printer output
        cp      003H                    ; CTRL-C ?
        ret     nz                      ; nope, quit
        ld      hl,(YF325)
        jp      XF1E8                   ; start abort handler in DOS memory

A5431:  ld      a,1
        ld      (YF23B),a
        ret

A5437:  xor     a
        ld      (YF23B),a
        ret

;       Subroutine      BDOS 0B (console status)
;       Inputs          
;       Outputs         ________________________

A543C:  call    A5412
        ld      a,000H
        ret     z
        or      0FFH
        ret

;       Subroutine      BDOS 01 (console input)
;       Inputs          
;       Outputs         ________________________

A5445:  call    A544E                   ; BDOS 8 (direct input)
        push    af
        call    A53A8                   ; console output
        pop     af
        ret

;       Subroutine      BDOS 08 (direct input)
;       Inputs          
;       Outputs         ________________________

A544E:  call    A541D
        jr      z,A544E
        ret

;       Subroutine      BDOS 06 (direct console i/o)
;       Inputs          A=0FFH for console input, A<>0FFH for console output
;       Outputs         A=input (console input)

A5454:  ld      a,e
        cp      0FFH                    ; console input ?
        jp      nz,A408F                ; console output, output to screen and quit
        call    A4034                   ; check if keyboardinput available
        jp      nz,A4078                ; yep, get keyboardinput and quit
        xor     a
        ret

;       Subroutine      BDOS 07 (MSXDOS direct input)
;       Inputs          
;       Outputs         ________________________

A5462:  jp      A4078                   ; get keyboardinput

;       Subroutine      BDOS 05 (printer output)
;       Inputs          
;       Outputs         ________________________

A5465:  ld      a,e
A5466:  push    af
        call    A5412
        pop     af
        jp      A409B                   ; output to printer

;       Subroutine      BDOS 03 (auxiliary input)
;       Inputs          
;       Outputs         ________________________

A546E:  call    A5412
        jp      XF371

;       Subroutine      BDOS 04 (auxiliary output)
;       Inputs          
;       Outputs         ________________________

A5474:  ld      a,e
A5475:  push    af
        call    A5412
        pop     af
        jp      XF374

; Programmers message, decoded in FAT entries (not needed)
; MSXDOS BY T PATERSON J SUZUKI   @

T547D:  defb    0EDH,08CH,093H,0EFH,00CH,088H,039H,040H
        defb    003H,070H,048H,097H,0F2H,0FCH,0BAH,080H
        defb    00AH,0CCH,0B5H,05EH,0AFH,029H,000H,000H

; unused code

        ret

A5496:  call    XF2B2
        call    A54C0                   ; get time and date values
        ld      a,c
        add     a,a
        add     a,a
        add     a,a
        rl      b
        add     a,a
        rl      b
        add     a,a
        rl      b
        srl     d
        or      d
        ld      e,a
        ld      d,b
        ld      bc,(YF249)
        ld      a,c
        add     a,a
        add     a,a
        add     a,a
        add     a,a
        add     a,a
        rl      b
        ld      c,a
        ld      a,(YF248)               ; current day
        or      c
        ld      c,a
        ret

A54C0:  call    A4179                   ; get date and time
        jr      c,A550B                 ; from clockchip,
        push    de
        push    hl
        ld      de,(YF24C)              ; days since 1-1-1980
        or      a
        sbc     hl,de
        pop     hl
        pop     de
        ret     z
        ld      (YF24C),hl
        push    bc
        push    de
        ld      c,l
        ld      b,h
        ld      de,4*365+1
        call    A492F                   ; divide
        ld      a,c
        add     a,a
        add     a,a
        add     a,a
        ld      b,000H
        ld      de,T5534
        call    A5515
        srl     a
        jr      nc,A54F2
        ld      de,200
        add     hl,de
A54F2:  call    A5523                   ; setup days in februari
        ld      a,001H
        ld      de,YF22B
        call    A5515
        ld      (YF249),a
        inc     l
        ld      a,l
        ld      (YF248),a               ; current day
A5505:  call    A5588
        pop     de
        pop     bc
        ret

A550B:  push    bc
        push    de
        ld      hl,(YF24A)
        call    A559D                   ; calculate days since 1-1-1980
        jr      A5505

A5515:  ex      de,hl
        ld      c,(hl)
        inc     hl
        ex      de,hl
        or      a
        sbc     hl,bc
        jr      c,A5521
        inc     a
        jr      A5515

A5521:  add     hl,bc
        ret

A5523:  call    XF2B5
        ld      (YF24A),a               ; year (offset)
A5529:  and     003H
        ld      a,28
        jr      nz,A5530
        inc     a
A5530:  ld      (YF22B+1),a
        ret

T5534:  defb    200,166,200,165,200,165,200,165

;       Subroutine      BDOS 2A (MSXDOS get date)
;       Inputs          
;       Outputs         ________________________

A553C:  xor     a
        ld      (YF306),a               ; no CP/M call
        call    A54C0                   ; get time and date values
        ld      hl,(YF24A)
        ld      de,1980
        add     hl,de
        ld      de,(YF248)              ; current day and month
        ld      a,(YF24E)
        ret

;       Subroutine      BDOS 2B (set date)
;       Inputs          
;       Outputs         ________________________

A5552:  ld      bc,-1980
        add     hl,bc
        jr      nc,A559A                ; year <1980, error
        ld      a,h
        or      a
        jr      nz,A559A                ; yearoffset not in 1 byte, error
        ld      a,l
        cp      120
        jr      nc,A559A                ; year >2099, error
        call    A5529                   ; setup febuari days
        inc     e
        dec     e
        jr      z,A559A                 ; day 0, error
        ld      a,d
        or      a
        jr      z,A559A                 ; month 0, error
        cp      12+1
        jr      nc,A559A                ; month >12, error
        push    hl
        ld      hl,YF22B-1
        add     a,l
        ld      l,a
        jr      nc,A5579
        inc     h
A5579:  ld      a,(hl)                  ; days in month
        pop     hl
        cp      e
        jr      c,A559A                 ; invalid day, error
        ld      (YF248),de              ; current day and month
        call    A559D                   ; calculate days since 1-1-1980
        call    A4115                   ; store date (clockchip or otherwise)
A5588:  ld      bc,(YF24C)              ; days since 1-1-1980
        ld      de,7
        inc     bc
        inc     bc
        call    A492F                   ; divide
        ld      a,l
        ld      (YF24E),a
        xor     a
        ret

A559A:  ld      a,0FFH
        ret

A559D:  ld      a,l                     ; year (offset)
        call    A5523                   ; setup days in februari
        ld      c,l
        srl     c
        srl     c                       ; /4
        ld      b,0
        ld      de,4*365+1
        call    A4916                   ; multiply
        ld      l,c
        ld      h,b
        ld      a,(YF24A)               ; year (offset)
        and     003H
        add     a,a
        ld      de,T5534
        ld      b,0
        inc     a
        call    A55D2
        ld      de,YF22B
        ld      a,(YF249)               ; current month
        call    A55D2
        ld      a,(YF248)               ; current day
        dec     a
        ld      c,a
        add     hl,bc
        ld      (YF24C),hl              ; days since 1-1-1980
        ret

A55D2:  dec     a
        ret     z
        ex      de,hl
        ld      c,(hl)
        inc     hl
        ex      de,hl
        add     hl,bc
        jr      A55D2

;       Subroutine      BDOS 2C (MSXDOS get time)
;       Inputs          
;       Outputs         ________________________

A55DB:  xor     a
        ld      (YF306),a               ; no CP/M call
        call    A54C0                   ; get time and date values
        ld      h,b
        ld      l,c
        xor     a
        ret

;       Subroutine      BDOS 2D (set time)
;       Inputs          
;       Outputs         ________________________

A55E6:  ld      b,h
        ld      c,l
        ld      a,b
        cp      24
        jr      nc,A559A
        ld      a,59
        cp      c
        jr      c,A559A
        cp      d
        jr      c,A559A
        ld      a,e
        cp      100
        jr      nc,A559A
        call    A4130                   ; store time (clockchip or otherwise)
        xor     a
        ret

;       Subroutine      BDOS 2E (set verify flag)
;       Inputs          
;       Outputs         ________________________

A55FF:  ld      a,e
        ld      (RAWFLG),a
        ret

;       Subroutine      Validate FCB filename
;       Inputs          HL = address of FCB+1,DE = destination
;       Outputs         ________________________
;       Remark          is copied to 0F1F4H

A5604:  ld      a,(hl)
        cp      " "
        scf
        ret     z                       ; filename that start with a space is illegal, quit
        ld      bc,00802H               ; first do the filename, then the fileextension
        cp      0E5H
        jr      nz,A5622                ; not the charcode also used as deleted file marker
        ld      a,005H
        ld      (de),a
        inc     hl
        inc     de
        dec     b                       ; use replacement charcode 005H, otherwise fileentry looks deleted
        ld      a,0E5H
        call    A5681                   ; is this a double byte 'header' char ?
        jr      nc,A5622                ; nope, no special action
        ld      a,(hl)
        ld      (de),a
        inc     hl
        inc     de
        dec     b                       ; yep, copy 'follow' char
A5622:  ld      a,(hl)
        call    A5681                   ; is this a double byte 'header' char ?
        jr      nc,A5631                ; nope, do upcasing and check
        ld      (de),a
        inc     hl
        inc     de                      ; copy 'header' char
        dec     b
        scf
        ret     z                       ; no 'follow' char, quit with error
        ld      a,(hl)
        jr      A5667                   ; copy 'follow' char and continue

A5631:  ld      a,(YF30E)
        and     a
        ld      a,(hl)
        jr      z,A564C                 ; japanese have no accent chars,
        cp      080H
        jr      c,A564C                 ; normal ASCII,
        cp      0BAH
        jr      nc,A564C
        push    hl                      ; 080H-0B9H accent chars
        push    bc
        ld      c,a
        ld      b,000H
        ld      hl,T5696-080H
        add     hl,bc
        ld      a,(hl)                  ; get the upcase version of the accent char
        pop     bc
        pop     hl
A564C:  cp      "a"
        jr      c,A5656
        cp      "z"+1
        jr      nc,A5656
        sub     020H                    ; lowercase char, make upcase
A5656:  cp      020H
        ret     c                       ; control code are illegal, quit with error
        push    hl
        push    bc
        ld      hl,T5677
        ld      bc,0000AH
        cpir                            ; one of the illegal chars ?
        pop     bc
        pop     hl
        scf
        ret     z                       ; yep, quit with error
A5667:  ld      (de),a                  ; copy char
        inc     hl
        inc     de
        djnz    A5622                   ; next char
        ld      b,003H
        dec     c
        jr      nz,A5622                ; now do the fileextension
        or      a                       ; flag no error
        ld      a,(hl)
        ld      (YF30C),a               ; save the FCB EX byte
        ret

T5677:  defb    '."/[]:+=;,'

;       Subroutine      check if double byte header char
;       Inputs          ________________________
;       Outputs         ________________________

A5681:  push    hl
        ld      hl,YF30F
        cp      (hl)
        ccf
        jr      nc,A5694                ; below (F30F), quit (not in range)
        inc     hl
        cp      (hl)
        jr      c,A5694                 ; below (F310), quit (in range 1)
        inc     hl
        cp      (hl)
        ccf
        jr      nc,A5694                ; below (F311), quit (not in range)
        inc     hl
        cp      (hl)
A5694:  pop     hl
        ret

;       Table 080H-0B9H accent upcase chars

T5696:  defb    080H,09AH,"E" ,"A" ,08EH,"A" ,08FH,080H
        defb    "E" ,"E" ,"E" ,"I" ,"I" ,"I" ,08EH,08FH
        defb    090H,092H,092H,"O" ,099H,"O" ,"U" ,"U"
        defb    "Y" ,099H,09AH,09BH,09CH,09DH,09EH,09FH
        defb    "A" ,"I" ,"O" ,"U" ,0A5H,0A5H,0A6H,0A7H
        defb    0A8H,0A9H,0AAH,0ABH,0ACH,0ADH,0AEH,0AFH
        defb    0B0H,0B0H,0B2H,0B2H,0B4H,0B4H,0B6H,0B6H
        defb    0B8H,0B8H

;       Subroutine      unsupported CP/M BDOS calls
;       Inputs          
;       Outputs         ________________________

A56D0:  xor     a
        ld      b,a
        ret

;       Subroutine      BDOS handler (for DiskBASIC)
;       Inputs          
;       Outputs         ________________________

A56D3:  ei
        ld      a,1
        ld      (YF306),a               ; assume CP/M compatible call
        ld      a,c
        cp      031H                    ; valid BDOS function ?
        jr      nc,A56D0                ; nope, unsupported BDOS call
        sub     011H                    ; Search First BDOS function ?
        jr      nz,A56E6
        ld      (YF307),de              ; yep, save address FCB for Search Next
A56E6:  dec     a                       ; Search Next BDOS function ?
        jr      nz,A56ED
        ld      de,(YF307)              ; yep, get saved address FCB Search First
A56ED:  push    hl
        ld      hl,T5700
        ex      (sp),hl                 ; after BDOS routine, fill HL in a CP/M compatible manner
        push    hl
        ld      hl,T570D
        ld      b,0
        add     hl,bc
        add     hl,bc
        ld      b,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,b
        ex      (sp),hl
        ret                             ; start BDOS routine

T5700:  push    af
        ld      a,(YF306)
        or      a                       ; CP/M compatible call
        jr      z,A570B                 ; no, leave HL alone
        pop     af
        ld      l,a
        ld      h,b                     ; CP/M compatible HL
        ret

A570B:  pop     af
        ret

T570D:  defw    A40A7,A5445,A53A7,A546E,A5474,A5465,A5454,A5462
        defw    A544E,XF1C9,A50E0,A543C,A41EF,A509F,A50D5,A4462
        defw    A456F,A4FB8,A5006,A436C,A4775,A477D,A461D,A4392
        defw    A504E,A50C4,A5058,A505D,A56D0,A56D0,A56D0,A56D0
        defw    A56D0,A4788,A4793,A501E,A50C8,A56D0,A47BE,A47B2
        defw    A47D1,A56D0,A553C,A5552,A55DB,A55E6,A55FF,A46BA
        defw    A4720

A576F:  call    INIHRD                  ; initialize diskhardware
        di
        ld      a,(DEVICE)		; disksystem initialization status
        and     a
        ret     m			; disksystem initialization canceled, quit
        jp      nz,A580C                ; disksystem initialization already started by an other disk interface, skip init
        ld      hl,HOKVLD
        bit     0,(hl)                  ; EXTBIO hook valid ?
        jr      nz,A578E
        set     0,(hl)
        ld      hl,EXTBIO
        ld      b,3*5
A5789:  ld      (hl),0C9H
        inc     hl
        djnz    A5789                   ; nop, init EXTBIO,DISINT and ENAINT hooks
A578E:  ld      hl,(BOTTOM)
        ld      de,0C001H
        rst     020H                    ; at least 16Kb RAM ?
        jr      nc,A57A3                ; nop, cancel disksystem initiazation
        ld      a,006H
        call    SNSMAT
        di
        rrca                            ; SHIFT key pressed ?
        jr      c,A57A9                 ; nop, cont
        ld      a,007H
        rst     018H                    ; beep
A57A3:  ld      a,0FFH
        ld      (DEVICE),a              ; disksystem initialization canceled
        ret

;       Subroutine      Initialize disksystem, first disk interface

A57A9:  ld      hl,0F380H+MYSIZE
        ld      de,XF1C9
        and     a
        sbc     hl,de                   ; bytes needed for static workarea+workarea driver
        call    nc,A5EE8                ; allocate memory (adjust BASIC areapointers)
        ret     c                       ; failed, quit
A57B6:  push    hl
        ld      hl,XF1C9-0F380H
        ld      bc,XF1C9
A57BD:  xor     a
        ld      (bc),a
        inc     bc
        inc     hl
        ld      a,l
        or      h
        jr      nz,A57BD                ; clear static workarea
        ld      (AUTLIN),hl             ; biggest sectorsize sofar = 0
        ld      b,4*2+4*3
        ld      hl,YFB21
A57CD:  ld      (hl),a
        inc     hl
        djnz    A57CD                   ; clear DRVTBL, DRVINT
        ld      hl,XF24F
        ld      b,069H
A57D6:  ld      (hl),0C9H
        inc     hl
        djnz    A57D6                   ; init disksystem hooks
        ld      a,0FFH                  ; MSM: was: 0DBH, replaced IN by RST38h
        ld      hl,0C9A8H
        ld      (XF365+0),a
        ld      (XF365+1),hl            ; read primairy slotregister entry
        ld      a,006H
        call    SNSMAT
        and     002H
        ld      (YF33F),a               ; saved CTRL key status for panthom drive
        ld      a,007H
        rst     018H                    ; beep
        ld      hl,T5807
        ld      de,H.RUNC
        ld      bc,5
        ldir
        call    A402D                   ; get slotid of this disk interface
        ld      (H.RUNC+1),a            ; init RUNC hook, to intialize further when the interpeter is initialized
        pop     de                      ; base disk hardware driver workarea
        jr      A5825

T5807:  rst     030H
        defb    0
        defw    A5897
        ret

;       Subroutine      Initialize disksystem, not the first disk interface

A580C:  ld      hl,YFB21                ; DRVTBL
        ld      b,4                     ; 4 disk interfaces
        xor     a                       ; number of drives = 0
A5812:  add     a,(hl)                  ; update number of drives
        jp      c,A5ECC                 ; invalid DRVTBL, halt system
        inc     hl
        inc     hl
        djnz    A5812
        cp      8                       ; 8 or more drives ?
        ret     nc                      ; yep, no more drives!
        ld      hl,MYSIZE               ; number of bytes for workarea hardware driver
        call    A5EE8                   ; allocate memory (adjust BASIC areapointers)
        ret     c                       ; failed, quit
        ex      de,hl

A5825:  call    A5FCD                   ; get my SLTWRK entry
        ld      (hl),e
        inc     hl
        ld      (hl),d                  ; save base workarea in SLTWRK
        ld      hl,(AUTLIN)
        ld      de,SECLEN
        rst     020H                    ; SECLEN sofar big enough ?
        jr      nc,A5838
        ld      (AUTLIN),de             ; nop, adjust
A5838:  ld      de,YFB21                ; DRVTBL
        ld      bc,00400H               ; 4 disk interfaces, number of drives = 0
A583E:  ld      a,(de)
        and     a
        jr      z,A584B                 ; free entry, use it
        add     a,c
        ld      c,a                     ; update number of drives
        inc     de
        inc     de
        djnz    A583E                   ; next entry
        jp      A5ECC                   ; none free, halt system

A584B:  ld      a,(YF33F)
        and     a
        ld      a,c                     ; phantom flag
A5850:  call    DRIVES                  ; query no. of drives
        add     a,l
        cp      8+1
        ld      a,l                     ; more as 8 drives ?
        jr      c,A585C                 ; nop, ok
        ld      a,8
        sub     c                       ; as much as possible
A585C:  push    bc
        ld      (de),a                  ; save drives
        inc     de
        call    A402D                   ; get slotid of this disk interface
        ld      (de),a                  ; save slotid disk interface
        pop     bc                      ; drivenumber
        ld      b,0
        ld      hl,YF355
        add     hl,bc
        add     hl,bc                   ; DPBTBL
        push    hl
        dec     de
        ld      a,(de)
        push    af
        ld      c,a                     ; drives
        ld      de,21
        call    A4916                   ; * size of DPB
        ld      l,c
        ld      h,b                     ; number of bytes for the DPBs
        call    A5EC8                   ; allocate memory (adjust BASIC areapointers, halt when error)
        ex      de,hl
        pop     af
        pop     hl
A587E:  ld      (hl),e
        inc     hl
        ld      (hl),d                  ; save in DPBTBL
        inc     hl
        push    hl
        ld      hl,DEFDPB
        ld      bc,21
        ldir                            ; initialize DPB
        pop     hl
        dec     a
        jr      nz,A587E                ; next drive
        call    INIENV                  ; initialize hardware driver workarea
        ld      hl,DEVICE
        inc     (hl)                    ; increase disk interface count
        ret

;       Subroutine      H.RUNC interceptor
;       Inputs          -
;       Outputs         -
;       Remark          Control is passed to this routine when the BASIC interpreter is initialized
;                       There are two ways: a BASIC program in ROM is started OR at the start of MSX-BASIC

A5897:  ld      hl,H.RUNC
        ld      b,5
A589C:  ld      (hl),0C9H
        inc     hl
        djnz    A589C                   ; clear RUNC hook
        ld      hl,DEVICE
        xor     a
        cp      (hl)
        ld      (hl),a                  ; clear diskinterface count
        ret     p                       ; already cleared, return control
A58A8:  call    A622D                   ; hook H.LOPD when H.CLEA is hooked (for register system bottom)
        ld      (YF348),a               ; master diskrom slotid
        ld      hl,A7397
        ld      de,XF1C9
        ld      bc,0006EH
        ldir                            ; initialize some static disksystem variables
        ld      hl,M0034
        ld      de,YF30F
        ld      bc,4
        ldir                            ; initialize double byte header char table
        ld      a,(IDBYT0)
        rrca
        rrca
        rrca
        rrca
        and     007H
        ld      (YF30E),a               ; date format
        ld      a,0FFH
        ld      (YF241),a               ; invalid datasector buffer
        ld      (YF246),a               ; invalid directorysector buffer
        ld      (YF24C+1),a             ; days since 1-1-1980 is 65280 (0FF00H) (somewhere in the year 2158), this is inpossible, so when no clockchip this is updated!
        ld      a,00DH
        ld      (YCONBF+130),a          ; ?? end marker con buffer
        ld      a,7
        ld      (YF345),a               ; max number of FCBs is 7
        ld      hl,1461
        ld      (YF33B),hl              ; default date when no clockchip is 1-1-1984
        ld      b,8
        ld      hl,XF368
A58F0:  ld      (hl),0C3H
        inc     hl
        inc     hl
        inc     hl
        djnz    A58F0                   ; initialize jumptable
        ld      hl,(AUTLIN)
        push    hl                      ; size of the biggest sector
        call    A5EC8                   ; allocate memory (adjust BASIC areapointers, halt when error)
        ld      (SECBUF),hl            ; allocate sectorbuffer
        pop     hl                      ; size of the biggest sector
        push    hl
        call    A5EC8                   ; allocate memory (adjust BASIC areapointers, halt when error)
        ld      (YF34F),hl              ; allocate datasector buffer
        pop     hl                      ; size of the biggest sector
        call    A5EC8                   ; allocate memory (adjust BASIC areapointers, halt when error)
        ld      (YF351),hl              ; allocate dirsector buffer
        ld      hl,YFB21                ; DRVTBL
        ld      b,4                     ; 4 disk interfaces
        xor     a                       ; number of drives = 0
A5916:  add     a,(hl)                  ; update number of drives
        jp      c,A5ECC                 ; invalid DRVTBL, halt system
        inc     hl
        inc     hl
        djnz    A5916
        cp      8+1                     ; more as 8 drives ?
        jp      nc,A5ECC                ; yep (DRVTBL corrupted ?), halt system
        ld      (YF347),a               ; drives in system
        ld      b,a                     ; number of drives
        ld      c,0                     ; drive 0
        ld      hl,YF355
A592C:  ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; DPB of drive
        inc     hl
        push    hl
        push    de
        pop     ix
        ld      (ix+0),c                ; set drivenumber in DPB
        inc     c
        push    bc
        ld      c,(ix+2)
        ld      b,(ix+3)                ; sectorsize
        ld      e,(ix+16)
        ld      d,0                     ; number of sectors per FAT
        call    A4916                   ; * FAT size
        inc     bc                      ; and a FAT buffer flag
        ld      l,c
        ld      h,b                     ; size of the FAT buffer
        call    A5EC8                   ; allocate memory (adjust BASIC areapointers, halt when error)
        ld      (YF349),hl              ; base system bottom sofar
        ld      (hl),0FFH               ; flag invalid FAT buffer
        inc     hl
        ld      (ix+19),l
        ld      (ix+20),h               ; pointer to FAT buffer
        pop     bc
        pop     hl
        djnz    A592C                   ; next drive
        ld      hl,XF327
        ld      (hl),03EH
        inc     hl
        ld      (hl),01AH
        ld      b,2*5-2
A5967:  inc     hl
        ld      (hl),0C9H
        djnz    A5967                   ; initialize MSX-serial hooks
        ld      hl,XF327
        ld      (XF371+1),hl
        ld      hl,XF32C
        ld      (XF374+1),hl
        ld      hl,XF331
        ld      (XF37D+1),hl            ; initialize jumptable
        ld      hl,M7D2F
        ld      a,(EXPTBL+0)
        call    RDSLT
        push    af
        inc     hl
        ld      a,(EXPTBL+0)
        call    RDSLT
        pop     de
        ld      h,a
        ld      l,d                     ; read startup screen address
        push    hl
        pop     ix
        ld      iy,(EXPTBL-1+0)
        ;call    CALSLT                  ; initialize BASIC screenmode
        LD      A,12  ;MSM: disable calling to BIOS/EXTBIO to set screen mode + cls
        RST     18h                      ; clear screen
        ; MSM: skip clockchip check for now
        NOP
        NOP
        NOP
        ;call    A40B8                   ; check for and initialize clockchip
        call    A5C6E                   ; initialize hooks
        ld      a,(EXPTBL+0)
        ld      (RAMAD0),a
        ld      (RAMAD1),a              ; assume no ram available for page 0 and 1
        call    A5F98
        ld      (RAMAD2),a              ; slotid of current page 2
        call    A5F95
        ld      (RAMAD3),a              ; slotid of current page 3
        ld      c,000H
        ; MSM - disable search of RAM in page 0
        ; RDVRM/WRVRM destroys page 0 bios in slot 1 and screws up
        ;call    A5E52                   ; search ram in page 0
        ;jr      c,A59C1
        nop
        nop
        nop
        ld      a, 1
        ld      (RAMAD0),a              ; found, set ram slotid page 0
A59C1:  ld      c,040H
        call    A5E52                   ; search ram in page 1
        jr      c,A59CB
        ld      (RAMAD1),a              ; found, set ram slotid page 1
A59CB:  ld      sp,0C200H               ; switch to a temporary stack, just above temp startbuffer
        ld      a,(H.STKE+0)
        cp      0C9H                    ; STKE hook set ?
        jr      z,A59DB                 ; nop, cont
        ld      ix,M7D17
        jr      A59ED                   ; skip BASIC extension ROMs and transfer control

A59DB:  ld      hl,SLTATR
        ld      b,040H
A59E0:  ld      a,(hl)
        add     a,a                     ; TEXT extension ?
        jr      c,A59E9                 ; yep, start it
        inc     hl
        djnz    A59E0
        jr      A59F3                   ; no TEXT extension ROM found,

A59E9:  ld      ix,M7E14                ; start BASIC program in ROM
A59ED:  call    A5C16                   ; initialize diskbasic
        jp      CALBAS

A59F3:  ld      hl,A5B3A
        push    hl                      ; if quit anywhere start diskbasic
        call    A5AE7                   ; read bootsector
        ret     c                       ; failed, start diskbasic
        call    A5ADB                   ; start bootcode with Cx reset (some disk can take control from here)
        ld      hl,(BOTTOM)
        ld      de,08000H
        rst     020H                    ; check if ram on both page 3 and 2
        ret     nz                      ; nope, start diskbasic
        ld      hl,RAMAD0
        ld      a,(EXPTBL+0)
        cp      (hl)
        ret     z                       ; no ram available on page 0, start diskbasic
        inc     hl
        cp      (hl)
        ret     z                       ; no ram available on page 1, start diskbasic

; MSXDOS requirement are met, try starting it

A5A11:  XOR     A
        CALL    A609A                   ; invalidate FAT buffer drive 0
        LD      HL,(YF349)
        LD      (YF34B),HL              ; bottom MSX-DOS system
        CALL    A5AE7                   ; try reading bootsector of drive 0
        JP      C,A5B3A                 ; error, start diskbasic
        LD      (YF346),A               ; flag bootable disk
        LD      A,(RAMAD0)
        LD      H,00H                   ; page 0
        CALL    C64C2                   ; calculate slot masks
        SCF
        CALL    P,C64A2                 ; slot is not expanded, change it
        CALL    C,C64AF                 ; slot is expanded, change it
        XOR     A
        LD      L,A
        LD      H,A
J5A36:  LD      (HL),A
        INC     L
        JR      NZ,J5A36                ; clear 0000-00FF
        LD      BC,L637B                ; size of the XFER routine
        CALL    A5EAD                   ; allocate MSXDOS memory (halt when error)
        LD      (XFER+1),HL
        EX      DE,HL
        LD      HL,I637B
        LDIR                            ; install XFER routine
        LD      BC,L63A1                ; size of the ENARAM and ENAKRN routine
        CALL    A5EAD                   ; allocate MSXDOS memory (halt when error)
        LD      E,L
        LD      D,H
        LD      (XF368+1),HL
        INC     HL
        INC     HL
        LD      (XF36B+1),HL
        LD      HL,I63A1
        LDIR                            ; install ENAKRN and ENARAM
        LD      BC,L63F4                ; size of the slotswitching routines
        CALL    A5EAD                   ; allocate MSXDOS memory (halt when error)
        PUSH    HL
        EX      DE,HL
        LD      HL,C63F4
        PUSH    HL
        LDIR                            ; install slot switching routines
        POP     BC
        POP     DE
        PUSH    DE
        LD      HL,R0116-C63F4+1
        ADD     HL,DE
        LD      (HL),LOW X003B
        INC     HL
        LD      (HL),HIGH X003B         ; update "store and change secundairy slotregister" routine
        LD      HL,T63BA
        CALL    A6306                   ; relocate slot switching routines
        LD      HL,I5B0F
        XOR     A
        LD      B,A
        LD      D,A
J5A84:  LD      E,(HL)
        CP      E
        JR      Z,J5A98                 ; end of table, continue
        INC     HL
        LD      C,(HL)
        INC     HL
        EX      (SP),HL
        ADD     HL,BC
        EX      DE,HL
        LD      (HL),0C3H
        INC     HL
        LD      (HL),E
        INC     HL
        LD      (HL),D
        EX      DE,HL
        EX      (SP),HL
        JR      J5A84                   ; make jumptable

J5A98:  POP     HL
        ;LD      HL,C655C
        NOP
        NOP
        NOP
        ;LD      DE,X003B
        NOP
        NOP
        NOP
        ;LD      BC,L655C
        NOP
        NOP
        NOP
        ;LDIR                            ; install slotswitching helper routines
        NOP
        NOP
        ;
        LD      BC,L6336                ; size of the interrupt routine
        CALL    A5EAD                   ; allocate MSXDOS memory (halt when error)
        PUSH    HL
        EX      DE,HL
        LD      HL,I6336
        PUSH    HL
        LDIR                            ; install interrupt routine
        POP     BC
        POP     DE
        PUSH    DE
        LD      HL,T632E
        CALL    A6306                   ; relocate interrupt routine
        POP     HL
        LD      A,0C3H
        LD      (KEYINT+0),A
        LD      (KEYINT+1),HL           ; KEYINT
        LD      DE,R0021-I6336+1
        ADD     HL,DE
        LD      DE,(YF34B)
        LD      (HL),E
        INC     HL
        LD      (HL),D                  ; start of KEYINT stack
        LD      BC,160                  ; size of the KEYINT stack
        CALL    A5EAD                   ; allocate MSXDOS memory (halt when error)
        LD      A,0C3H
        CALL    A5C62                   ; enable XFER, ENAKRN and ENARAM routines
        SCF
A5ADB:  LD      HL,YF323                ; address diskerror handler pointer
        LD      DE,XF368                ; ENAKRN routine
        LD      A,(YF340)               ; coldboot flag
        JP      YC000+01EH              ; start bootloader

;       Subroutine      read bootsector of drive 0 and validate
;       Inputs          ________________________
;       Outputs         ________________________

A5AE7:  LD      A,(DEFDPB+1)
        LD      C,A                     ; default mediadescriptor
        LD      B,1                     ; 1 sector
        LD      HL,(YF351)              ; use the dir sectorbuffer
        PUSH    HL
        XOR     A                       ; drive 0, read
        LD      E,A
        LD      D,A                     ; sector 0
        CALL    PHYDIO                  ; read sector
        LD      A,0FFH
        LD      (YF246),A               ; dirsector buffer is invalid
        POP     HL
        RET     C                       ; error, quit
        LD      A,(HL)
        LD      DE,YC000
        LD      BC,256
        LDIR                            ; copy bootloader to C000
        CP      0EBH
        RET     Z                       ; valid bootloader, quit
        CP      0E9H
        RET     Z                       ; valid bootloader, quit
        SCF
        RET

I5B0F:  DEFB    LOW RDSLT,C63F4-C63F4
        DEFB    LOW WRSLT,C6415-C63F4
        DEFB    LOW CALLF,C6443-C6415
        DEFB    LOW CALSLT,C6455-C6443
        DEFB    LOW ENASLT,J649C-C6455
        DEFB    0

T5B1A:  defb    0,"AUTOEXECBAS",0
L5B1A   EQU     $-T5B1A

T5B27:  defb    'RUN"AUTOEXEC.BAS',0
L5B27   EQU     $-T5B27

T5B38:  defw    A5B92                   ; start DiskBASIC in direct mode

;       Subroutine      Start DiskBASIC

A5B3A:  call    A5C60                   ; disable XFER,ENARAM,ENAKRN routines
        ld      hl,T5B27
        ld      de,BUF+10
        ld      bc,L5B27
        ldir                            ; copy RUN"AUTOEXEC.BAS in BUF
        ld      hl,YF340
        ld      a,(hl)
        and     a                       ; is this a warm boot ?
        ld      (hl),h                  ; next boot is a warm boot
        jr      nz,A5B6E                ; yep, no autoexec.bas but a parameter from MSXDOS ?

;       Subroutine      try to start AUTOEXEC.BAS

        ld      (YF346),a               ; flag CALL SYSTEM invalid
        ld      hl,T5B38
        ld      (YF323),hl              ; setup disk errorhandler
        ld      hl,T5B1A
        ld      de,BUF+10+L5B27
        ld      bc,37                   ; a bit odd, should be L5B15
        push    de
        ldir                            ; setup FCB for autoexec.bas
        pop     de
        call    A4462                   ; open FCB
        inc     a
        jr      z,A5B92                 ; error, start DiskBASIC in direct mode (with date input)
        jr      A5BA0                   ; no error, start DiskBASIC and run AUTOEXEC.BAS

A5B6E:  ld      a,(WBOOT)
        cp      0C3H                    ; MSXDOS active ?
        jr      nz,A5B9C                ; nope, start DiskBASIC in direct mode
        ld      hl,00080H
        ld      b,(hl)
        inc     b
        dec     b
        jr      z,A5B9C                 ; no parameter specified (after the BASIC command), start DiskBASIC in direct mode
A5B7D:  inc     hl
        ld      a,(hl)
        cp      " "
        jr      nz,A5B87
        djnz    A5B7D                   ; remove spaces in front
        jr      A5B9C                   ; no parameter specified, just start diskbasic

A5B87:  xor     a
        ld      c,b
        ld      b,a
        ld      de,BUF+14
        ldir                            ; copy file name after the RUN" at BUF+10
        ld      (de),a                  ; and a end of line marker
        jr      A5BA0                   ; start DiskBASIC and run specified basicfile

;       Subroutine      start DiskBASIC in direct mode (cold boot)

A5B92:  ld      sp,0C200H               ; switch to a temporary stack
        ld      a,(YF338)
        and     a
        call    z,A5D3F                 ; no clockchip, ask the date

;       Subroutine      start DiskBASIC in direct mode

A5B9C:  xor     a
        ld      (BUF+13),a              ; make it a ordinary RUN at BUF+10

;       Subroutine      start DiskBASIC

A5BA0:  ld      sp,0C200H               ; switch to a temporary stack
        ld      a,(RAMAD2)
        ld      h,080H
        call    ENASLT                  ; ram on page 2
        ld      a,(EXPTBL+0)
        ld      h,000H
        call    ENASLT                  ; rom-bios on page 0
        ld      hl,(BOTTOM)
        xor     a
        ld      (hl),a                  ; before the program always a end of line marker
        inc     hl
        ld      (TXTTAB),hl             ; start of basictext space
        ld      (hl),a
        inc     hl
        ld      (hl),a                  ; end of program marker
        inc     hl
        ld      (VARTAB),hl             ; start of the variablespace
        ld      hl,0FFFFH
        ld      (CURLIN),hl             ; interpreter in direct mode
        call    A5C16                   ; initialize diskbasic
        ld      sp,(STKTOP)
        ld      a,0FFH
        ld      (CNSDFG),a              ; enable function keys
        ld      a,00CH
        rst     018H                    ; clear screen
        ld      ix,M7D31
        call    CALBAS                  ; display BASIC startscreen
        call    A5F86
        defb    13,10
        defb    "Disk BASIC version 1.0",13,10
        defb    0
        ld      hl,M4173
        push    hl                      ; execute RUN command
        ld      hl,BUF+10-1
        push    hl                      ; basicpointer
        ld      hl,BUF+64
        push    hl
        ld      (hl),0E1H
        inc     hl
        ld      (hl),0C9H               ; pop the basicpointer when returning
        ld      a,(EXPTBL+0)
        ld      h,040H
        jp      ENASLT                  ; enable basic-rom on page 1

;       Subroutine      initialize DiskBASIC environment
;       Inputs          ________________________
;       Outputs         ________________________

A5C16:  ld      hl,T72AE
        ld      (YF323),hl              ; setup diskerror handler
        ld      hl,T5C6C
        ld      (YF325),hl              ; setup abort handler
        ld      hl,(YF349)
        ld      (HIMEM),hl
        ld      a,(YF345)
        ld      c,a
        ld      b,0                     ; number of FCBs
        ld      de,37
        call    A4916                   ; * 37
        call    A5EB8                   ; allocate memory (adjust HIMEM, halt when error)
        ld      (YF353),hl		; Disk BASIC i/o channel FCBs
        ld      bc,25                   ; size of the BLOAD/BSAVE code
        call    A5EB8                   ; allocate memory (adjust HIMEM, halt when error)
        ld      (XF377+1),hl            ; setup BLOAD jumpentry
        ex      de,hl
        ld      hl,T62ED
        ldir                            ; copy bsave/bload patch code
        ld      hl,0FFF5H
        add     hl,de
        ld      (XF37A+1),hl            ; setup BSAVE jumpentry
        ld      a,(YF348)
        ld      de,0FFF9H
        add     hl,de
        ld      (hl),a
        ld      de,0000EH
        add     hl,de
        ld      (hl),a
        call    A5F5F                   ; setup i/o channels
                                        ; disable XFER..
A5C60:  ld      a,0C9H
A5C62:  ld      (XF368+0),a
        ld      (XF36B+0),a
        ld      (XFER+0),a
        ret

T5C6C:  defw    A40A7                   ; restart basic

;       Subroutine      initialize hooks
;       Inputs          ________________________
;       Outputs         ________________________

A5C6E:  ld      hl,T62E8
        ld      de,H.POSD
        ld      bc,5
        ldir
        ld      hl,T5C96
A5C7C:  ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      a,e
        or      d
        ret     z
        ex      de,hl
        ld      (hl),0F7H
        inc     hl
        ld      a,(YF348)
        ld      (hl),a
        inc     hl
        ex      de,hl
        ldi
        ldi
        ld      a,0C9H
        ld      (de),a
        jr      A5C7C

T5C96:  defw    H.DSKO,A6B96
        defw    H.DSKI,A6B75
        defw    H.NAME,A6F20
        defw    H.KILL,A6F00
        defw    H.COPY,A707B
        defw    H.DSKF,A7061
        defw    H.LSET,A6CD7
        defw    H.RSET,A6CD6
        defw    H.FIEL,A6C49
        defw    H.MKI,A6DAF
        defw    H.MKS,A6DB2
        defw    H.MKD,A6DB5
        defw    H.CVI,A6DD7
        defw    H.CVS,A6DDA
        defw    H.CVD,A6DDD
        defw    H.GETP,A66A4
        defw    H.NOFO,A66B3
        defw    H.NULO,A66FC
        defw    H.NTFL,A68D0
        defw    H.BINS,A690E
        defw    H.BINL,A6939
        defw    H.FILE,A6E88
        defw    H.DGET,A6BDA
        defw    H.FILO,A688E
        defw    H.INDS,A6819
        defw    H.LOC,A700D
        defw    H.LOF,A7009
        defw    H.EOF,A6E70
        defw    H.BAKU,A6875
        defw    H.PARD,A7323
        defw    H.NODE,A737C
        defw    H.ERRP,A71D3
        defw    H.PHYD,A6055
        defw    H.FORM,A60B0
        defw    XF331,A56D3
        defw    0

T5D24:  defb    1,048H,"-",1,041H,"-",1,047H,"):",0
T5D2F:  defb    "M-D-Y):",0
T5D37:  defb    "D-M-Y):",0

;       Subroutine      Ask for systemdate
;       Inputs          ________________________
;       Outputs         ________________________

A5D3F:  ld      (BUF+98),sp
        ld      a,20
        ld      (BUF+100),a             ; size of lineinput buffer
A5D48:  call    A5F86
        defb    13,10,"Enter date (",0
        nop
        nop
        nop
        nop                             ; room for expansion ??
        nop                             ; extra 5 bytes
        ld      a,(YF30E)
        cp      1
        ld      hl,T5D24
        jr      c,A5D71                 ; japanese Y-M-D
        ld      hl,T5D2F
        jr      z,A5D71                 ; european D-M-Y
        ld      hl,T5D37                ; american M-D-Y
A5D71:  call    A5F8C                   ; print string
        ld      hl,T5E4B
        ld      (YF325),hl              ; setup abort handler
        ld      de,BUF+100
        call    A50E0                   ; BDOS 0A (buffered console input)
        ld      hl,BUF+100+2
        ld      a,(hl)
        cp      00DH
        ret     z                       ; empty input, do not set date, quit
        ld      a,(YF30E)
        and     a
        jr      nz,A5D9F                ; no japanese
        call    A5E0D                   ; get date year
        call    A5DE9                   ; check for seperator and get date number (month)
        ld      d,c
        ld      a,(hl)
        inc     hl
        cp      b                       ; check if same seperator as the first one
        jr      nz,A5DCB                ; nope, error
        call    A5DF8                   ; get date number (day)
        ld      e,c
        jr      A5DB9

A5D9F:  call    A5DF8                   ; get date number (day european, month american)
        ld      d,c
        call    A5DE9                   ; check for seperator and get date number (month european, day american)
        ld      e,c
        ld      a,(hl)
        inc     hl
        cp      b                       ; check if same seperator as the first one
        jr      nz,A5DB1                ; nope, error
        call    A5E0D                   ; get date year
        jr      A5DB9

A5DB1:  push    de
        call    A553C                   ; get date
        push    hl
        pop     ix
A5DB8:  pop     de
A5DB9:  ld      a,(YF30E)
        cp      002H
        jr      c,A5DC3                 ; european or japanese, month already in register D
        ld      a,e
        ld      e,d
        ld      d,a                     ; month in register D, day in register E
A5DC3:  push    ix
        pop     hl
        call    A5552                   ; set date
        or      a
        ret     z                       ; no error, quit
A5DCB:  ld      sp,(BUF+98)
        call    A5F86
        defb    13,10,"Invalid date",0
        nop
        nop
        nop
        nop                             ; room for expansion ??
        nop                             ; extra 5 bytes
        jp      A5D48                   ; try again

A5DE9:  ld      a,(hl)
        inc     hl
        ld      b,a
        cp      "/"
        jr      z,A5DF8
        cp      "."
        jr      z,A5DF8
        cp      "-"
        jr      nz,A5DCB

A5DF8:  call    A5E41                   ; is digit ?
        jr      c,A5DCB                 ; nope, error
        ld      c,a                     ; save digit
        call    A5E41                   ; is digit ?
        ret     c                       ; nope, quit
        push    af
        ld      a,c
        add     a,a
        add     a,a
        add     a,c
        add     a,a
        ld      c,a                     ; first digit *10
        pop     af
        add     a,c
        ld      c,a                     ; + second digit
        ret

A5E0D:  call    A5DF8                   ; get date number
        ld      b,c
        call    A5E41                   ; is digit (is year more as 2 digit number) ?
        jr      c,A5E2C                 ; nope, make it a 4 digit number
        dec     hl
        call    A5DF8                   ; get date number
        push    hl
        push    bc
        ld      c,b
        ld      b,000H
        push    de
        ld      de,100
        call    A4916                   ; * 100
        pop     de
        pop     hl
        ld      h,000H
        jr      A5E3B

A5E2C:  push    hl
        ld      c,b
        ld      b,0
        ld      hl,1900
        ld      a,c
        cp      80
        jr      nc,A5E3B                ; >= 80 means 19xx
        ld      hl,2000                 ; >80 means 20xx
A5E3B:  add     hl,bc
        push    hl
        pop     ix
        pop     hl
        ret

A5E41:  ld      a,(hl)
        sub     030H
        ret     c
        cp      00AH
        ccf
        ret     c
        inc     hl
        ret

T5E4B:  defw    A5E4D

A5E4D:  ld      sp,(BUF+98)
        ret

A5E52:  LD      HL,EXPTBL
        LD      B,4
        XOR     A
J5E58:  AND     03H
        OR      (HL)
J5E5B:  PUSH    BC
        PUSH    HL
        LD      H,C
J5E5E:  LD      L,10H
J5E60:  PUSH    AF
        CALL    RDSLT
        CPL
        LD      E,A
        POP     AF
        PUSH    DE
        PUSH    AF
        CALL    WRSLT
        POP     AF
        POP     DE
        PUSH    AF
        PUSH    DE
        CALL    RDSLT
        POP     BC
        LD      B,A
        LD      A,C
        CPL
        LD      E,A
        POP     AF
        PUSH    AF
        PUSH    BC
        CALL    WRSLT
        POP     BC
        LD      A,C
        CP      B
        JR      NZ,J5E9A
        POP     AF
        DEC     L
        JR      NZ,J5E60
        INC     H
        INC     H
        INC     H
        INC     H
        LD      C,A
        LD      A,H
        CP      40H
        JR      Z,J5E96
        CP      80H
        LD      A,C
        JR      NZ,J5E5E
J5E96:  LD      A,C
        POP     HL
        POP     HL
        RET

J5E9A:  POP     AF
        POP     HL
        POP     BC
        AND     A
        JP      P,J5EA7
        ADD     A,4
        CP      90H
        JR      C,J5E5B
J5EA7:  INC     HL
        INC     A
        DJNZ    J5E58
        SCF
        RET

;       Subroutine      allocate MSXDOS memory (halt when error)
;       Inputs          HL = number of bytes to allocate
;       Outputs         

A5EAD:  ld      hl,(YF34B)
        and     a
        sbc     hl,bc
        ld      (YF34B),hl              ; new top of MSXDOS
        jr      A5EC1

;       Subroutine      allocate memory (adjust HIMEM, halt when error)
;       Inputs          HL = number of bytes to allocate
;       Outputs         

A5EB8:  ld      hl,(HIMEM)
        and     a
        sbc     hl,bc
        ld      (HIMEM),hl              ; new top of system
A5EC1:  jr      c,A5ECC                 ; new top below zero, halt system
        ld      a,h
        cp      0C2H                    ; new top below 0C200H ?
        jr      A5ECB

;       Subroutine      allocate memory (adjust BASIC areapointers, halt when error)
;       Inputs          HL = number of bytes to allocate
;       Outputs         

A5EC8:  call    A5EE8                   ; allocate memory (adjust BASIC areapointers)
A5ECB:  ret     nc
A5ECC:  call    A5F86
        defb    12
T5ED0:  defb    "No enough memory",0
        nop
        nop
        nop
        nop                             ; room for expansion ??
        nop                             ; extra 5 bytes
        di
        halt

;       Subroutine      allocate memory (adjust BASIC areapointers)
;       Inputs          HL = number of bytes to allocate
;       Outputs         Cx set if out of memory

A5EE8:  ld      a,l
        or      h                       ; size = 0 ?
        ret     z                       ; yep, quit
        xor     a
        sub     l
        ld      l,a
        ld      a,0
        sbc     a,h
        ld      h,a
        ld      c,l
        ld      b,h                     ; - size
        add     hl,sp
        ccf
        ret     c                       ; stack would become below zero, quit
        ld      de,(BOTTOM)
        sbc     hl,de
        ret     c                       ; stack would become below BOTTOM, quit
        ld      a,h
        cp      HIGH 512
        ret     c                       ; less then 512 bytes for stack, quit
        push    bc
        ld      hl,0
        add     hl,sp
        ld      e,l
        ld      d,h                     ; top of stack
        add     hl,bc
        push    hl                      ; new top of stack
        ld      hl,(STKTOP)
        and     a
        sbc     hl,de
        ld      c,l
        ld      b,h                     ; size of the stack
        inc     bc
        pop     hl
        ld      sp,hl                   ; new stack
        ex      de,hl
        ldir                            ; copy stack content to new location
        pop     bc
        ld      hl,(HIMEM)
        add     hl,bc
        ld      (HIMEM),hl              ; adjust HIMEM
        ld      de,-(2*256+2*9+2*2)
        add     hl,de
        ld      (FILTAB),hl             ; 1 user i/o channel + 1 system i/o channel
        ex      de,hl
        ld      hl,(MEMSIZ)
        add     hl,bc
        ld      (MEMSIZ),hl             ; adjust MEMSIZ
        ld      hl,(NULBUF)
        add     hl,bc
        ld      (NULBUF),hl             ; adjust NULBUF
        ld      hl,(STKTOP)
        add     hl,bc                   ; adjust STKTOP
A5F3B:  ld      (STKTOP),hl
        dec     hl
        dec     hl
        ld      (SAVSTK),hl
        ld      l,e
        ld      h,d
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      a,2                     ; 2 i/o channels (1 user, 1 system)
A5F4B:  ex      de,hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        inc     hl
        ex      de,hl
        ld      bc,7
        ld      (hl),b                  ; i/o channel not open
        add     hl,bc
        ld      (hl),b                  ; clear i/o channel flags
        ld      bc,256+9-7
        add     hl,bc
        dec     a
        jr      nz,A5F4B
        ret

A5F5F:  ld      a,1
        ld      (MAXFIL),a
        ld      hl,(HIMEM)
        ld      de,-(2*256+2*9+2*2)
        add     hl,de
        ld      (FILTAB),hl
        ld      e,l
        ld      d,h
        dec     hl
        dec     hl
        ld      (MEMSIZ),hl
        ld      bc,200
        and     a
        sbc     hl,bc
        push    hl
        ld      hl,2*2+9
        add     hl,de
        ld      (NULBUF),hl
        pop     hl
        jr      A5F3B

A5F86:  ex      (sp),hl
        call    A5F8C                   ; print string
        ex      (sp),hl
        ret

A5F8C:  ld      a,(hl)
        inc     hl
        and     a
        ret     z
        call    A408F                   ; output to screen
        jr      A5F8C

;       Subroutine      get slotid of page 3
;       Inputs          -
;       Outputs         A = slotid

A5F95:  ld      b,6
        defb    021H                    ; LD HL,xxxx (skips next instruction)

;       Subroutine      get slotid of page 2
;       Inputs          -
;       Outputs         A = slotid

A5F98:  ld      b,4
        call    XF365
        push    bc
A5F9E:  rrca
        djnz    A5F9E
        call    A5FEC
        pop     bc
        or      (hl)
        ld      c,a
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        dec     b
        dec     b
A5FAE:  rrca
        djnz    A5FAE
        jr      A5FBE

;       Subroutine      get my slotid
;       Inputs          -
;       Outputs         A = slotid

A5FB3:  call    A5FE7
        or      (hl)
        ret     p                       ; non expanded slot, quit
        ld      c,a
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
A5FBE:  and     00CH
        or      c
        ret

;       Subroutine      get my disk hardware driver workarea
;       Inputs          -
;       Outputs         HL = IX = pointer to workarea
;       Remark          used by the disk hardware driver

GETWRK:
A5FC2:  call    A5FCD                   ; get my SLTWRK entry
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a                     ; pointer to workarea
        push    hl
        pop     ix
        ret

;       Subroutine      get my SLTWRK entry
;       Inputs          -
;       Outputs         HL = pointer to SLTWRK entry

A5FCD:  call    A5FE7                   ; get my primairy slot
        add     a,a
        add     a,a
        add     a,a
        scf
        adc     a,a                     ; primary slot*4 + 1
        ld      c,a
        ld      a,(hl)
        add     a,a
        sbc     a,a
        and     00CH                    ; 0 for non expanded, 0CH for expanded
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        and     (hl)
        or      c
        add     a,a                     ; word entries
        ld      hl,SLTWRK
        jr      A5FF3

;       Subroutine      get my EXPTBL entry
;       Inputs          -
;       Outputs         HL = pointer to SLTWRK entry

A5FE7:  call    XF365
        rrca
        rrca
A5FEC:  and     003H
        ld      hl,EXPTBL
A5FF1:  ld      b,000H
A5FF3:  ld      c,a
        add     hl,bc
        ret

;	  Subroutine register interrupt handler
;	     Inputs  HL = interrupt handler
;	     Outputs ________________________
;            Remark  used by the disk hardware driver

SETINT:
A5FF6:  ld      a,(H.TIMI+0)
        cp      0C9H
        jr      z,A6012                 ; H.TIMI not hooked, skip saving H.TIMI
        push    hl
        ld      a,(DEVICE)              ; current disk interface number
        ld      hl,YFB29
        call    A5FF1
        add     hl,bc
        add     hl,bc                   ; get pointer to DRVINT entry
        ex      de,hl
        ld      hl,H.TIMI+1
        ld      c,3
        ldir                            ; save slotid and address (assumes that is hooked by a CALLF!)
        pop     hl
A6012:  di
        ld      a,0F7H
        ld      (H.TIMI+0),a
        ld      (H.TIMI+2),hl           ; disk hardware driver interrupt handler
        ld      a,0C9H
        ld      (H.TIMI+4),a
        call    A402D                   ; get slotid of this disk interface
        ld      (H.TIMI+1),a            ; slotid of this disk interface
        ret

;       Subroutine      call orginal interrupt handler
;       Inputs          -
;       Outputs         -
;       Remark          used by the disk hardware driver

PRVINT:
A6027:  push    af                      ; store VDP status register
        call    A402D                   ; get slotid of this disk interface
        ld      b,4
        ld      de,YFB29
        ld      hl,YFB21+1
A6033:  cp      (hl)                    ; is this my DRVTBL entry ?
        jr      z,A603F                 ; yep, get the saved interrupt handler and jump to it
        inc     de
        inc     de
        inc     de
        inc     hl
        inc     hl
        djnz    A6033                   ; next DRVTBL and DRVINT entry
A603D:  pop     af                      ; restore VDP status register
        ret                             ; quit

A603F:  ex      de,hl
        ld      a,(hl)
        and     a                       ; disk interface has saved a interrupt handler ?
        jr      z,A603D                 ; nope, quit
        push    af
        pop     iy                      ; slotid
        inc     hl
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        push    bc
        pop     ix                      ; address interrupt handler
        pop     af                      ; restore VDP status register
        jp      CALSLT                  ; call interrupt handler

;       Subroutine      read disksector
;       Inputs          A = driveid, B = number of sectors, C = mediadescriptor, DE = start sector, HL = transferaddress
;       Outputs         -

A6052:  and     a
        defb    038H                    ; JR C,xx (skips next instruction)

;       Subroutine      write disksector
;       Inputs          A = driveid, B = number of sectors, C = mediadescriptor, DE = start sector, HL = transferaddress
;       Outputs         -

A6054:  scf

;       Subroutine      PHYDIO BIOS call (H.PHYD)
;       Inputs          A = driveid, B = number of sectors, C = mediadescriptor, DE = start sector, HL = transferaddress
;       Outputs         -

A6055:  push    ix
        push    iy
        push    hl
        push    af
        call    A6086                   ; get diskdriverparameters
        ld      l,a
        pop     af                      ; restore flags (Cx)
        ld      a,l
        ld      ix,T4010
        jr      A607B

A6067:  push    ix
        ld      ix,T4013
        jr      A6075

A606F:  push    ix
        ld      ix,T4016
A6075:  push    iy
        push    hl
        call    A6086                   ; get diskdriverparameters
A607B:  pop     hl
        push    hl
        call    CALSLT
        jp      A636E

; Unused code
; looks like the previous jump is a patch, may be the ei instruction at the end of the patch was forgotten ??
; patch code does not fit here, 1 byte too less

        nop
        nop
        nop

;       Subroutine      get diskdriverparameters
;       Inputs          A=driveid
;       Outputs         IYH=slotid diskdriver, H=slotid diskdriver, A=local driveid diskdriver

A6086:  ld      (YF33F),a               ; save driveid (for PROMPT)
        ld      hl,YFB21                ; disk interface table
A608C:  sub     (hl)
        jr      c,A6093
        inc     hl
        inc     hl
        jr      A608C

A6093:  add     a,(hl)                  ; driveid disk interface
        inc     hl
        ld      h,(hl)                  ; slotid disk interface
        push    hl
        pop     iy
        ret

;       Subroutine      mark FAT buffer as invalid
;       Inputs          A=driveid
;       Outputs         ________________________

A609A:  ld      hl,YF355
        call    A5FF1
        add     hl,bc
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to DPB
        ld      hl,00013H
        add     hl,de
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        dec     de
        ex      de,hl
        ld      (hl),0FFH
        ret

;       Subroutine      FORMAT BIOS call (H.FORM)
;       Inputs          
;       Outputs         ________________________

A60B0:  and     a                       ; use free BASIC memory as formatbuffer

;       Subroutine      format disk
;       Inputs          Cx reset = use free BASIC memory as formatbuffer, Cx set = use specified buffer, HL = start of buffer, BC = size of buffer
;       Outputs         ________________________

A60B1:  ld      (YF339),sp              ; save stackpointer for abort
        call    nc,A62D6                ; get formatbuffer in free BASIC memory
        push    hl
        push    bc
        ld      a,(YF347)
        dec     a                       ; only 1 diskdrive ?
        jr      z,A6101                 ; yep, skip drivename input and use driveid 0
A60C0:  call    A5F86
        defb    "Drive name? (",0
        nop
        nop
        nop
        nop                             ; room for expansion ??
        nop                             ; extra 5 bytes
        ld      a,(YF347)
        ld      b,a                     ; number of diskdrives
        ld      a,"A"                   ; starts with "A" drive
        jr      A60E5

A60DE:  push    af
        ld      a,","
        call    A408F                   ; output to screen
        pop     af
A60E5:  call    A408F                   ; output to screen
        inc     a
        djnz    A60DE                   ; next drive
        call    A5F86
        defb    ") ",0
        call    A6195                   ; get fresh consoleinput (CTRL-C aborts)
        call    A62C6                   ; print input and CR/LF
        and     0DFH                    ; upcase
        sub     "A"
        ld      hl,YF347
        cp      (hl)                    ; driveletter valid ?
        jr      nc,A60C0                ; nope, ask again
A6101:  call    A609A                   ; mark FAT buffer as invalid
        call    A6086                   ; get diskdriverparameters
        push    iy
        push    af
        push    iy
        ld      ix,T4019
        call    CALSLT                  ; call CHOICE diskdriver
        ld      a,l
        or      h                       ; choice string ?
        jr      z,A613E                 ; nope, skip choice input and use choice 0 (default format)
        pop     af
A6118:  push    af
        call    RDSLT
        and     a
        jr      z,A6126
        call    A408F                   ; output to screen
        inc     hl
        pop     af
        jr      A6118                   ; print choice string

A6126:  pop     af
        call    A5F86
        defb    "? ",0
A612D:  call    A6195                   ; get fresh consoleinput (CTRL-C aborts)
        sub     "1"
        cp      009H                    ; input "1"-"9" ?
        jr      nc,A612D                ; nope, ask again
        add     a,"1"
A6138:  call    A62C6                   ; print input and CR/LF
        sub     "0"                     ; choice 1-9
        push    af
A613E:  call    A6174                   ; prompt and get fresh consoleinput (CTRL-C aborts)
        pop     af
        pop     de
        pop     iy
        pop     bc
        pop     hl
        ld      ix,T401C
        call    CALSLT                  ; call DSKFMT diskdriver
        ld      hl,T61E0
        jr      nc,A615D                ; no error, print "format complete" and quit
        ld      hl,T6248
        call    A5FF1                   ; get errorstring entry
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a                     ; pointer to errorstring
A615D:  call    A62C9                   ; print CR/LF
        call    A5F8C                   ; print string
        jp      A62C9                   ; print CR/LF

;       Subroutine      get fresh keyboardinput
;       Inputs          
;       Outputs         ________________________

A6166:  xor     a
        ld      (YF336),a               ; no saved input
        ld      ix,KILBUF
        call    A40AB                   ; KILBUF BIOS call
        jp      A4078                   ; get keyboardinput

A6174:  call    A5F86
        defb    "Strike a key when ready ",0
        nop
        nop
        nop
        nop                             ; room for expansion ??
        nop                             ; extra 5 bytes
A6195:  call    A6166                   ; get fresh keyboardinput
        cp      003H
        ret     nz
        ld      sp,(YF339)
        call    A5F86
        defb    13,10,"Aborted",0
        ret

T61AD:  defb    "Write protected",0
T61BD:  defb    "Not ready",0
T61C7:  defb    "Disk error",0
T61D2:  defb    "Bad parameter",0
T61E0:  defb    "Format complete",0

; unused code ??

        nop

A61F1:  ld      de,TEMPST+30
        ld      hl,(TEMPPT)
        ld      (DAC+2),hl
        ld      a,3
        ld      (VALTYP),a
        call    VMOVE                   ; copy stringdescriptor
        ld      de,TEMPST+30+3
        rst     020H
        ld      (TEMPPT),hl
        ret     nz
        jp      A72F7

A620D:  ld      hl,YFB21
        ld      b,4
A6212:  inc     hl
        ld      a,(hl)                  ; slotid of this interface
        push    af
        pop     iy                      ; IYH = slotid
        inc     hl
        push    hl
        push    bc
        ld      hl,T401F
        push    hl
        pop     ix                      ; IX = MTOFF
        and     a                       ; entry for interface used ?
        call    nz,RDSLT                ; yep, read MTOFF entry
        and     a                       ; entry for interface used AND a valid MTOFF entry ?
        call    nz,CALSLT               ; yep, call MTOFF of interface
        pop     bc
        pop     hl
        djnz    A6212                   ; next interface
        ret

A622D:  ld      a,(H.CLEA+0)
        cp      0C9H
A6232:  jr      z,A6245
A6234:  ld      hl,T72F2
        ld      de,H.LOPD
        ld      bc,00005H
        ldir
        call    A402D                   ; get slotid of this disk interface
        ld      (H.LOPD+1),a
A6245:  jp      A402D                   ; get slotid of this disk interface

T6248:  defw    T61AD           ; dskfmt error 0
        defw    T61BD           ; dskfmt error 2
        defw    T61C7           ; dskfmt error 4
        defw    T61C7           ; dskfmt error 6
        defw    T61C7           ; dskfmt error 8
        defw    T61C7           ; dskfmt error 10
        defw    T61D2           ; dskfmt error 12
        defw    T5ED0           ; dskfmt error 14
        defw    T61C7           ; dskfmt error 16

PROMPT:
L625A:  ld      a,(YF33F)               ; driveid
        add     a,"A"
        call    XF24F                   ; PROMPT hook
        push    af
        call    A5F86
        defb    13,10,"Insert diskette for drive ",0
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop                             ; room for expansion
        nop                             ; extra 10 bytes
        pop     af
        call    A408F                   ; output to screen
        call    A5F86
        defb    ":",13,10,"and strike a key when ready",0
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop
        nop                             ; room for expansion
        nop                             ; extra 10 bytes
A62BD:  call    A6166                   ; get fresh keyboardinput
        cp      003H
        jr      z,A62BD
        jr      A62C9                   ; print CR/LF

A62C6:  call    A408F                   ; output to screen
A62C9:  push    af
        ld      a,00DH
        call    A408F                   ; output to screen
        ld      a,00AH
        call    A408F                   ; output to screen
A62D4:  pop     af
        ret

A62D6:  ld      hl,0FF00H
        add     hl,sp                   ; leave room for the stack
        ld      de,(STREND)             ; end of used BASIC memory
        xor     a
        sbc     hl,de
        ld      c,l
        ld      b,h                     ; size of buffer
        ex      de,hl                   ; start of buffer
        ret     nc
        ld      c,a
        ld      b,a                     ; not enough room, size=0
        ret

;       Subroutine      first character filespecifier is 000H-039H (H.POSD)
;       Inputs          
;       Outputs         ________________________

T62E8:  inc     sp
        inc     sp
        jp      PARDEV+8                ; skip strong cassette devicecheck

T62ED:  ld      a,d
        cp      9                       ; diskdevice ?
        jp      nc,BLOAD+3              ; nope, resume BLOAD
        rst     030H
        defb    000H
        defw    A6AA7                   ; call BLOAD for diskdevice
        push    hl
        jp      BLDFIN                  ; finish BLOAD

        ld      a,d
        cp      9                       ; diskdevice ?
        jp      nc,BSAVE+3              ; nope, resume BSAVE
        rst     030H
        defb    000H
        defw    A69E7
        ret

A6306:  PUSH    DE                      ; DE = destination, HL = reloctable, BC = source
        EX      DE,HL
        AND     A
        SBC     HL,BC                   ; delta dest - source
        PUSH    HL
        POP     IX                      ; delta in IX
        EX      DE,HL
        POP     DE
J6310:  LD      C,(HL)                  ; DE = destination, HL = ptr start reloctable, BC = source
        INC     HL
        LD      B,(HL)                  ; BC holds contents reloctable entry
        INC     HL
        LD      A,C
        AND     B
        INC     A
        RET     Z                       ; ffffh to signal end of relocation table
        PUSH    DE                      ; DE = destination, HL = ptr to next entry reloctable, BC reloctable entry                
        EX      DE,HL   
        ADD     HL,BC                   ; destination + reloctable entry
        INC     HL                      ; +1
        LD      C,(HL)
        INC     HL
        LD      B,(HL)                  ; BC = current contents of destination memory
        PUSH    HL                      
        PUSH    IX
        POP     HL                      ; HL = delta
        ADD     HL,BC                   ; calculate new value
        LD      C,L
        LD      B,H                     ; BC is new value
        POP     HL                      ; HL = ptr to next entry reloctable
        LD      (HL),B
        DEC     HL
        LD      (HL),C                  ; update contents of destination memory with new value
        EX      DE,HL
        POP     DE
        JR      J6310                   ; next entry

T632E:  DEFW    R000C-I6336
        DEFW    R001E+1-I6336
        DEFW    R002F-I6336
        DEFW    0FFFFH

I6336:  PUSH    IX
        PUSH    IY
        PUSH    HL
        PUSH    DE
        PUSH    BC
        PUSH    AF
        EXX
        EX      AF,AF'
        PUSH    AF
        PUSH    HL
R000C:  LD      HL,(D635E+1)
        LD      A,L
        OR      H
        POP     HL
        LD      IX,KEYINT
        LD      IY,(EXPTBL+0-1)
        JR      NZ,J6375
        POP     AF
R001E:  LD      (D635E+1),SP
R0021:  LD      SP,0
        CALL    CALSLT
        DI
D635E:  LD      SP,0
        PUSH    HL
        LD      HL,0
R002F:  LD      (D635E+1),HL
        POP     HL
J6369:  EX      AF,AF'
        EXX
        POP     AF
        POP     BC
        POP     DE
A636E:  POP     HL
        POP     IY
        POP     IX
        EI
        RET

J6375:  POP     AF
        CALL    CALSLT
        JR      J6369

L6336   EQU     $-I6336

I637B:  PUSH    AF
        PUSH    HL
        PUSH    DE
        PUSH    BC
        CALL    A402D                   ; get slotid of this disk interface
        PUSH    AF
        LD      H,40H
        LD      A,(RAMAD1)
        CALL    ENASLT
        POP     AF
        POP     BC
        POP     DE
        POP     HL
        LDIR
        PUSH    HL
        PUSH    DE
        PUSH    BC
        CALL    XF368
        LD      H,40H
        CALL    ENASLT
        POP     BC
        POP     DE
        POP     HL
        POP     AF
        RET

L637B   EQU     $-I637B

I63A1:  JR      J63A9
_63A3:  PUSH    AF
        LD      A,(RAMAD1)
        JR      J63AD

J63A9:  PUSH    AF
        LD      A,(YF348)
J63AD:  PUSH    HL
        PUSH    DE
        PUSH    BC
        LD      H,40H
        CALL    ENASLT
        POP     BC
        POP     DE
        POP     HL
        POP     AF
        RET

L63A1   EQU     $-I63A1

T63BA:  DEFW    C63F4-C63F4
;        DEFW    R0003-C63F4
;        DEFW    R0017-C63F4
;        DEFW    R001C-C63F4
;        DEFW    R0022-C63F4
;        DEFW    R0025-C63F4
;        DEFW    R0039-C63F4
;        DEFW    R003F-C63F4
;        DEFW    R0069-C63F4
;        DEFW    R006C-C63F4
;        DEFW    R007E-C63F4
;        DEFW    R0094-C63F4
;        DEFW    J649C-C63F4
;        DEFW    R00AB-C63F4
;        DEFW    R00BC-C63F4
;        DEFW    J6404-C63F4
;        DEFW    R0013-C63F4
;        DEFW    J6425-C63F4
;        DEFW    R0034-C63F4
;        DEFW    J646C-C63F4
;        DEFW    R007B-C63F4
;        DEFW    J64A9-C63F4
;        DEFW    R00B8-C63F4
;        DEFW    J651E-C63F4
;        DEFW    R0131-C63F4
;        DEFW    J652C-C63F4
;        DEFW    R0145-C63F4
;        DEFW    C6541-C63F4
        DEFW    0FFFFH


;       Subroutine      MSXDOS RDSLT
;       Inputs          ________________________
;       Outputs         ________________________
C63F4:
        ld ix, 0x0006
        DB 0x5b ; .LIL
        rst 38h
        ret
        DS 06415h - $
;C63F4:  CALL    C64C2                   ; calculate slot masks
;R0003:  JP      M,J6404                 ; slot is expanded,
;        RST     38h                     ; MSM: replaced IN A by RST38h
;        DB      0a8h
;        ;IN      A,(0A8H)
;        LD      D,A
;        AND     C
;        OR      B
;        CALL    RDPRIM
;        LD      A,E
;        RET

;J6404:  CALL    C6511                   ; page 0 and same primary slot of helper routines ?
;R0013:  JP      Z,J651E                 ; yep, use workaround code RDSLT in page 3
;        PUSH    HL
;R0017:  CALL    C64E7
;        EX      (SP),HL
;        PUSH    BC
;R001C:  CALL    C63F4                   ; MSXDOS RDSLT
;        JR      J6436

;       Subroutine      MSXDOS WRSLT
;       Inputs          ________________________
;       Outputs         ________________________
C6415:
        ld ix, 0x0008
        DB 0x5b ; .LIL
        rst 38h
        ret
        DS 06443h - $

;C6415:  PUSH    DE
;R0022:  CALL    C64C2                   ; calculate slot masks
;R0025:  JP      M,J6425                 ; slot is expanded,
;        POP     DE
;        RST     38h                     ; MSM: replaced IN A by RST38h
;        DB      0a8h
;        ;IN      A,(0A8H)
;        LD      D,A
;        AND     C
;        OR      B
;        JP      WRPRIM

;J6425:  CALL    C6511                   ; page 0 and same primary slot of helper routines ?
;R0034:  JP      Z,J6524                 ; yep, use workaround code WRSLT in page 3
;        EX      (SP),HL
;        PUSH    HL
;R0039:  CALL    C64E7
;        POP     DE
;        EX      (SP),HL
;        PUSH    BC
;R003F:  CALL    C6415                   ; MSXDOS WRSLT
;J6436:  POP     BC
;        EX      (SP),HL
;        PUSH    AF
;        LD      A,B
;        AND     3FH
;        OR      C
;        CALL    X0046                   ; restore secundairy slotregister
;        POP     AF
;        POP     HL
;        RET

;       Subroutine      MSXDOS CALLF
;       Inputs          ________________________
;       Outputs         ________________________

C6443:  EX      (SP),HL
        PUSH    AF
        PUSH    DE
        LD      A,(HL)
        PUSH    AF
        POP     IY
        INC     HL
        LD      E,(HL)
        INC     HL
        LD      D,(HL)
        INC     HL
        PUSH    DE
        POP     IX
        POP     DE
        POP     AF
        EX      (SP),HL

;       Subroutine      MSXDOS CALSLT
;       Inputs          ________________________
;       Outputs         ________________________

C6455:
        ; now slotid is in IY and address is in IX
        ; first save IX
        DB 0x5b ; .LIL
        rst 18h
        ;  electron os calslt
        ld ix, 0x000a
        DB 0x5b ; .LIL
        rst 38h
        ret
        DS 0649ch-$
;C6455:  EXX
;        EX      AF,AF'
;        PUSH    IY
;        POP     AF
;        PUSH    IX
;        POP     HL
;R0069:  CALL    C64C2                   ; calculate slot masks
;R006C:  JP      M,J646C                 ; slot is expanded,
;        RST     38h                     ; MSM: replaced IN A by RST38h
;        DB      0a8h
;        ;IN      A,(0A8H)
;        PUSH    AF
;        AND     C
;        OR      B
;        EXX
;        JP      CLPRIM

;J646C:  CALL    C6511                   ; page 0 and same primary slot of helper routines ?
;R007B:  JP      Z,J652C                 ; yep,use workaround code CALSLT in page 3
;R007E:  CALL    C64E7
;        PUSH    AF
;        POP     IY
;        PUSH    HL
;        PUSH    BC
;        LD      C,A
;        LD      B,00H
;        LD      A,L
;        AND     H
;        OR      D
;        LD      HL,SLTTBL
;        ADD     HL,BC
;        LD      (HL),A                  ; update secundairy slotregister copy in SLTTBL
;        PUSH    HL
;        EX      AF,AF'
;        EXX
;R0094:  CALL    C6455                   ; MSXDOS CALSLT
;        EXX
;        EX      AF,AF'
;        POP     HL
;        POP     BC
;        POP     DE
;        LD      A,B
;        AND     3FH
;        OR      C
;        DI
;        CALL    X004B                   ; restore secundairy slotregister
;        LD      (HL),E
;        EX      AF,AF'
;        EXX
;        RET

;       Subroutine      MSXDOS ENASLT
;       Inputs          ________________________
;       Outputs         ________________________

J649C:  
    JR C64A2
    DS 064a2h-$
C64A2:
    ld ix, 0x000c
    DB 0x5b ; .LIL
    rst 38h
    ret

    DS 064AFh-$
C64AF:   ; do not support expanded slots
    ret
    DS 064C2h-$
;J649C:  CALL    C64C2                   ; calculate slot masks
;R00AB:  JP      M,J64A9                 ; slot is expanded,
;C64A2:  RST     38h                     ; MSM: replaced IN A by RST38h
;        DB      0a8h
;        ;IN      A,(0A8H)
;        AND     C
;        OR      B
;        RST     28h     ; MSM: replace out (a8h),a by RST28
;        DB      0a8h
;        ;OUT     (0A8H),A
;        RET

;J64A9:  CALL    C6511                   ; page 0 and same primary slot of helper routines ?
;R00B8:  JP      Z,C6541                 ; yep, use workaround code ENASLT in page 3
;C64AF:  PUSH    HL
;R00BC:  CALL    C64E7
;        LD      C,A
;        LD      B,00H
;        LD      A,L
;        AND     H
;        OR      D
;        LD      HL,SLTTBL
;        ADD     HL,BC
;        LD      (HL),A                  ; update secundairy slotregister copy in SLTTBL
;        POP     HL
;        LD      A,C
;        JR      J649C                   ; MSXDOS ENASLT

;       Subroutine      calculate slot masks
;       Inputs          A = slotid, HL = address
;       Outputs         ________________________

C64C2:  DI
        PUSH    AF
        LD      A,H
        RLCA
        RLCA
        AND     03H
        LD      E,A
        INC     E
        LD      A,0C0H
J64CD:  RLCA
        RLCA
        DEC     E
        JR      NZ,J64CD
        LD      E,A
        CPL
        LD      C,A
        POP     AF
        PUSH    AF
        AND     03H
        LD      B,A
        INC     B
        LD      A,0ABH
J64DD:  ADD     A,55H
        DJNZ    J64DD
        LD      D,A
        AND     E
        LD      B,A
        POP     AF
        AND     A
        RET

;       Subroutine      __________________________
;       Inputs          ________________________
;       Outputs         ________________________

;C64E7:  PUSH    AF
;        LD      A,D
;        AND     0C0H
;        LD      C,A
;        POP     AF
;        PUSH    AF
;        LD      D,A
;        RST     38h                     ; MSM: replaced IN A by RST38h
;        DB      0a8h
;        ;IN      A,(0A8H)
;        LD      B,A
;        AND     3FH
;        OR      C
;        PUSH    AF
;        LD      A,D
;        RRCA
;        RRCA
;        AND     03H
;        LD      D,A
;        INC     D
;        LD      A,0ABH
;J64FF:  ADD     A,55H
;        DEC     D
;        JR      NZ,J64FF
;        AND     E
;        LD      D,A
;        LD      A,E
;        CPL
;        LD      H,A
;        POP     AF
;R0116:  CALL    C655C                   ; store and change secundairy slotregister
;        POP     AF
;        AND     03H
;        RET
R0116: ; MSM
        DS 0655Ch-$

;       Subroutine      __________________________
;       Inputs          ________________________
;       Outputs         ________________________

;C6511:  LD      C,A                     ; store slotid
;        LD      A,E
;        CP      03H                     ; page 0 ?
;        LD      A,C                     ; restore slotid
;        RET     NZ                      ; not page 0, quit
;        LD      A,(RAMAD0)
;        AND     E                       ; get primairy slot from slotid helper routines
;        CP      B                       ; same as requested ?
;        LD      A,C                     ; restore slotid
;        RET

;       Subroutine      workaround code RDSLT
;       Inputs          ________________________
;       Outputs         ________________________

;J651E:  CALL    C6549                   ; switch page 0 (same primairy slot)
;        LD      E,(HL)                  ; read byte from slot
;        JR      C6529

;       Subroutine      workaround code WRSLT
;       Inputs          ________________________
;       Outputs         ________________________

;J6524:  POP     DE
;R0131:  CALL    C6549                   ; switch page 0 (same primairy slot)
;        LD      (HL),E                  ; write byte to slot

;       Subroutine      __________________________
;       Inputs          ________________________
;       Outputs         ________________________

;C6529:  LD      A,B                     ; stored secundairy slotregister
;        JR      J6557                   ; restore secundairy slotregister and quit

;       Subroutine      workaround code CALSLT
;       Inputs          ________________________
;       Outputs         ________________________

;J652C:  CALL    C6541                   ; switch page 0 (same primairy slot)
;        PUSH    HL
;        PUSH    BC
;        EX      AF,AF'
;        EXX
;        CALL    CLPRM1                  ; call routine in slot
;        EXX
;        EX      AF,AF'
;        POP     BC
;R0145:  CALL    C6529
;        POP     HL
;        LD      (HL),B                  ; update secundairy slotregister copy in SLTTBL
;        EX      AF,AF'
;        EXX
;        RET

;       Subroutine      workaround code ENASLT
;       Inputs          ________________________
;       Outputs         ________________________

;C6541:  CALL    C6549                   ; switch page 0 (same primairy slot)
;        LD      HL,SLTTBL
;        LD      (HL),D                  ; update secundairy slotregister copy in SLTTBL (BUG, assumes primairy slot 0!)
;        RET

;       Subroutine      switch page 0 (same primairy slot)
;       Inputs          A = slotid
;       Outputs         ________________________

;C6549:  RRCA
;        RRCA
;        AND     03H
;        LD      D,A                     ; get secundiary slot from slotid
;        LD      A,(YFFFF)
;        CPL
;        LD      B,A                     ; store current secundairy slot register
;        AND     0FCH
;        OR      D
;        LD      D,A                     ; new secundairy slot register, page 0 switched
;J6557:  LD      (YFFFF),A
;        LD      A,E
;        RET

L63F4   EQU     $-C63F4

C655C: ; MSM

;       Subroutine      store and change secundairy slotregister (helper routine at 003BH)
;       Inputs          ________________________
;       Outputs         ________________________

;C655C:  RST     28h     ; MSM: replace out (a8h),a by RST28
;        DB      0a8h
;        ;OUT     (0A8H),A                ; make secundairy slotregister accessable (switch page 3)
;        LD      A,(YFFFF)
;        CPL
;        LD      L,A                     ; store current secundairy slotregister
;        AND     H
;        OR      D
;        JR      J656F                   ; change secundairy slotregister, restore primairy slotregister and quit

;       Subroutine      restore secundairy slotregister (helper routine at 0046H)
;       Inputs          ________________________
;       Outputs         ________________________

;_6567:  RST     28h     ; MSM: replace out (a8h),a by RST28
;        DB      0a8h
;        ;OUT     (0A8H),A                ; make secundairy slotregister accessable (switch page 3)
;        LD      A,L
;        JR      J656F                   ; change secundairy slotregister, restore primairy slotregister and quit

;       Subroutine      restore secundairy slotregister (helper routine at 004BH)
;       Inputs          ________________________
;       Outputs         ________________________

;_656C:  RST     28h     ; MSM: replace out (a8h),a by RST28
;        DB      0a8h
;        ;OUT     (0A8H),A                ; make secundairy slotregister accessable (switch page 3)
;        LD      A,E
;J656F:  LD      (YFFFF),A
;        LD      A,B
;        RST     28h     ; MSM: replace out (a8h),a by RST28
;        DB      0a8h
;        ;OUT     (0A8H),A
;        RET

L655C   EQU     $-C655C

        DS 06576h-$
A6576:  ld      a,(H.GETP+0)
        cp      0C9H
        scf
        ret     z
        push    hl
        call    A402D                   ; get slotid of this disk interface
        ld      hl,YF348
        cp      (hl)                    ; is this the master diskrom ?
        jr      nz,A65AD                ; nope, only try the disk interface OEM statements
        ld      hl,T65B1
A658A:  ld      de,PROCNM
A658D:  ld      a,(de)
        cp      (hl)
        inc     de
        inc     hl
        jr      nz,A65A3
        and     a
        jr      nz,A658D
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        ex      (sp),hl
        call    A72DA                   ;
        scf
        ccf
        ret

A65A1:  inc     hl
        ld      a,(hl)
A65A3:  and     a
        jr      nz,A65A1
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        and     a
        jr      nz,A658A
A65AD:  pop     hl
        jp      OEMSTA

T65B1:  defb    "SYSTEM",0
        defw    A65C4
        defb    "FORMAT",0
        defw    A65DC
        defb    0

A65C4:  ret     nz
        ld      a,(YF346)
        and     a
        jp      z,A7315
        ld      ix,CLSALL
        call    A731E                   ; close all i/o channels
        call    TOTEXT
        call    ERAFNK
        jp      A5A11                   ; start MSXDOS

A65DC:  push    hl
        call    z,A60B1
        pop     hl
        and     a
        ret

A65E3:  ld      a,d
A65E4:  dec     a
        ret     p
        ld      a,(YF247)
        ret

A65EA:  push    hl
        push    de
        push    bc
        call    A65E3                   ; convert to driveid
        ld      c,a
        ld      hl,(FILTAB)             ; I/O channel pointer table
        ld      a,(MAXFIL)              ; number of I/O channels
A65F7:  push    af
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        inc     hl
        push    hl
        ex      de,hl                   ; pointer to I/O channel
        ld      a,(hl)
        and     a                       ; channel in use ?
        jr      z,A662E                 ; nope, check next
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        cp      9                       ; channel in use by a disk device ?
        jr      nc,A662E                ; nope, check next
        dec     hl
        dec     hl
        ld      d,(hl)
        dec     hl
        ld      e,(hl)                  ; pointer to FCB
        ld      a,(de)                  ; DR byte of FCB
        call    A65E4                   ; convert to driveid
        cp      c                       ; same as the requested one ?
        jr      nz,A662E                ; nope, check next
        inc     de
        ex      de,hl
        ld      de,FILNAM
        ld      b,11
A661E:  ld      a,(de)
        cp      "?"
        jr      z,A6626                 ; wildcard, treat as equal
        cp      (hl)
        jr      nz,A662E                ; filename not equal, check next
A6626:  inc     de
        inc     hl
        djnz    A661E                   ; next char
        pop     hl
        pop     hl
        jr      A66A0                   ; quit with Zx set (file open)

A662E:  pop     hl
        pop     af
        dec     a
        jp      p,A65F7                 ; next channel
        jr      A66A0                   ; quit with Zx reset (file not open)

A6636:  push    hl
        push    de
        push    bc
        ld      a,d
        ld      (BUF+10),a
        call    A6657                   ; copy FILNAM to FCB
        call    A6645                   ; search file
        jr      A66A0

A6645:  ld      hl,BUF+84
        ld      (YF23D),hl              ; transferaddress in BUF
        ld      de,BUF+10
        xor     a
        ld      (BUF+10+12),a           ; clear EX byte of FCB
        call    A4FB8                   ; search for first
        inc     a
        ret

A6657:  ld      de,BUF+10+1
A665A:  ld      hl,FILNAM
        ld      bc,11
        ldir
        ret

;       Subroutine      take control from caller (move parameters on stack)
;       Inputs          IX = returnaddress replacement, IYH = number of bytes to move
;       Outputs         ________________________

;       This is what the stack looks like at entry:
;
;       prim    exp
;       +0      +0      returnaddress A6663 caller
;       +2      +2      callf BIOS registers
;       +6      +14     returnaddress CALLF caller
;       +8      +16     returnaddress hook caller


A6663:  ei
        push    hl
        push    de
        push    bc
        push    af
        ld      a,(YF348)
        add     a,a
        ld      hl,16
        jr      nc,A6673
        ld      l,16+8
A6673:  add     hl,sp
        push    ix
        pop     bc
        ld      (hl),c
        inc     hl
        ld      (hl),b
        ld      hl,10
        add     hl,sp
        ex      de,hl
        jr      A668D

A6681:  push    iy
        pop     bc
A6684:  ld      c,(hl)
        ld      a,(de)
        ld      (hl),a
        ld      a,c
        ld      (de),a
        inc     hl
        inc     de
        djnz    A6684
A668D:  ld      a,(YF348)
        add     a,a
        ld      hl,18
        jr      nc,A6698
        ld      l,18+8
A6698:  add     hl,sp
        ld      a,e
        sub     l
        ld      a,d
        sbc     a,h
        jr      c,A6681
A669F:  pop     af
A66A0:  pop     bc
        pop     de
        pop     hl
        ret

;       Subroutine      get pointer to i/o channel (H.GETP)
;       Inputs          
;       Outputs         ________________________

A66A4:  ld      ix,RETRTN
        ld      iy,00200H
        call    A6663                   ; take control from caller (move parameters on stack)
        pop     hl
        ld      a,(hl)
        and     a			; Zx set if i/o channel is not open
        ret

;       Subroutine      OPEN statement expander (H.NOFO)
;       Inputs          
;       Outputs         ________________________

A66B3:  ei
        ld      bc,256
        ld      (YF33D),bc              ; default recordsize 256
        call    A72DA                   ; at end of statement ?
        ld      a,e
        ret     z                       ; yes, quit
        push    af
        push    hl
        ld      a,(YF348)
        add     a,a
        ld      hl,12
        jr      nc,A66CD
        ld      l,12+8
A66CD:  add     hl,sp
        ld      a,(hl)
        cp      4                       ; random mode ?
        jp      nz,A7318                ; nope, syntax error
        inc     hl
        ld      a,(hl)
        cp      9                       ; disk device ?
        jp      nc,A7318                ; nope, syntax error
        pop     hl
        call    A72D0
        defb    0FFH
        call    A72D0
        defb    092H
        call    A72D0
        defb    0EFH                    ; check for LEN=
        ld      ix,INTID2
        call    A731E                   ; evaluate word operand and check for 0-32767 range
        dec     de
        inc     d
        dec     d                       ; should be 1-256
        jp      nz,A7315                ; nope,
        inc     de
        ld      (YF33D),de              ; recordsize
        pop     af
        ret

;       Subroutine      open i/o channel (H.NULO)
;       Inputs          HL = i/o channel pointer
;       Outputs         ________________________

A66FC:  ei
        ret     nc                      ; not for a disk device, return control
        ld      ix,RETRTN
        ld      iy,00400H
        call    A6663                   ; take control from caller (move parameters on stack)
        call    A6FA9                   ; validate filename
        call    A65EA                   ; is file already open in one of the I/O channels ?
        jp      z,A7309                 ; yep, file already open error
        ld      (PTRFIL),hl             ; interpreter input/output device = i/o channel pointer
        ld      a,e
        cp      4			; file mode = random i/o ?
        jr      z,A6721                 ; yep, recordsize already set
        ld      bc,1
        ld      (YF33D),bc              ; all others use recordsize 1
A6721:  pop     af
        push    af                      ; I/O channel number
        push    hl
        push    de
        ld      hl,YF345
        cp      (hl)                    ; do I have a FCB for this i/o channel ?
        jp      nc,A730C                ; nope, bad file number error
        ld      bc,37
        ld      e,a
        ld      d,b
        ld      hl,(YF353)              ; base of the Disk BASIC i/o channel FCBs
        call    A491C                   ; base + 37 * channelnumber
        xor     a
        ld      hl,12
        add     hl,bc
        ld      (hl),a                  ; reset EX byte of FCB
        pop     de
        pop     hl
        inc     hl
        ld      (hl),c
        inc     hl
        ld      (hl),b                  ; pointer to FCB
        inc     hl
        ld      (hl),a                  ; no backup char
        inc     hl
        ld      (hl),d                  ; devicenumber
        inc     hl
        inc     hl
        ld      (hl),a                  ; current bufferoffset = 0
        call    A6636                   ; search file
        push    bc
        push    de
        ld      a,d
        ld      (bc),a                  ; devicenumber
        ld      e,c
        ld      d,b
        inc     de
        call    A665A                   ; copy FILNAM to FCB
        pop     de
        pop     bc
        ld      a,e
        jr      nz,A6778                ; file exists,
        and     086H                    ; file not exists and not binsav, random or output mode ?
        jp      z,A7312                 ; yep, file not found error
A6761:  push    de
        push    bc
        ld      e,c
        ld      d,b
        call    A461D                   ; create file
        and     a
        jp      nz,A71AB                ; failed, error
        pop     hl
        call    A67FE                   ; setup FCB fields
        pop     de
        ld      hl,(PTRFIL)		; i/o channel pointer
        ld      (hl),e                  ; filemode, I/O channel open
A6775:  pop     af
        pop     hl
        ret

A6778:  cp      8
        jr      z,A67C1                 ; append mode,
        cp      2
        jr      z,A6761                 ; output mode, create file (overwrites!)
        cp      080H
        jr      z,A6761                 ; binsav mode, create file (overwrites!)
        push    de
        push    bc
        ld      e,c
        ld      d,b
        call    A4462                   ; open fcb
        pop     hl
        call    A67FE                   ; setup FCB fields
        pop     de
        ld      hl,(PTRFIL)		; i/o channel pointer
        ld      (hl),e                  ; filemode, I/O channel open
        ld      a,e
        cp      4                       ; random mode ?
        jr      z,A6775                 ; yep, quit
        push    hl
        ld      hl,FLBMEM
        xor     a
        cp      (hl)                    ; in raw mode ?
        ld      (hl),a
        pop     hl
        jr      nz,A6775                ; yep, quit
        ld      bc,6
        add     hl,bc
        push    hl
        ld      (hl),0FFH               ; position 255, so next get char fills buffer
        ld      hl,(PTRFIL)		; i/o channel pointer
        call    A682A                   ; get char from I/O channel
        pop     hl
        dec     hl
        dec     hl
        dec     hl
        ld      (hl),a
        cp      0FFH
        jr      nz,A6775                ; yep, quit
        inc     hl
        inc     hl
        inc     hl
        inc     hl
        ld      (hl),080H
A67BF:  jr      A6775                   ; quit

A67C1:  push    bc
        ld      e,c
        ld      d,b
        call    A4462                   ; open fcb
        pop     hl
        push    hl
        call    A67FE                   ; setup FCB fields
        ld      hl,(PTRFIL)		; i/o channel pointer
        ld      (hl),1                  ; first in sequential input mode
        ld      bc,6
        add     hl,bc
        ld      (hl),0FFH               ; position 255, so next get char fills buffer
        ld      hl,(PTRFIL)		; i/o channel pointer
A67DA:  push    hl
        call    A682A                   ; get char from I/O channel
        pop     hl
        jr      nc,A67DA                ; not at the end of file, continue
        ld      (hl),2                  ; continue in sequential output mode
        pop     hl
        ld      bc,00021H
        add     hl,bc                   ; to the Rx field
        ld      c,004H
        push    hl
        scf
A67EC:  ld      a,(hl)
        sbc     a,b
        ld      (hl),a
        inc     hl
        dec     c
        jr      nz,A67EC                ; decrease by 1
        pop     hl
        inc     c
        jr      nc,A67F9                ; Make Rx a multiply of 256
        ld      c,004H                  ; Rx = 0
A67F9:  call    A6813
        jr      A67BF                   ; quit

A67FE:  ld      bc,12
        add     hl,bc
        ld      (hl),b                  ; clear EX byte
        inc     hl
        ld      (hl),b                  ; clear S1 byte
        inc     hl
        ld      bc,(YF33D)
        ld      (hl),c
        inc     hl
        ld      (hl),b                  ; user recordsize = recordsize
        ld      bc,17
        add     hl,bc
        ld      c,5
A6813:  ld      (hl),b
        inc     hl
        dec     c
        jr      nz,A6813                ; clear CR byte and Rx bytes
        ret

;       Subroutine      input from i/o channel (H.INDS)
;       Inputs          
;       Outputs         ________________________

A6819:  ld      ix,RETRTN
        ld      iy,00600H
        call    A6663                   ; take control from caller (move parameters on stack)
        call    A682A                   ; get char from I/O channel
        jp      A66A0                   ; quit

A682A:  push    hl
        ld      a,(hl)
        cp      1                       ; input mode ?
        jp      nz,A7199                ; nope, bad file mode error
        ld      e,l
        ld      d,h
        inc     hl
        inc     hl
        inc     hl
        ld      a,(hl)
        and     a                       ; backup char ?
        jr      nz,A6866                ; yep, use that
        inc     hl
        inc     hl
        inc     hl
        inc     (hl)                    ; update counter
        ld      a,(hl)
        inc     hl
        inc     hl
        inc     hl                      ; to the buffer
        jr      nz,A6861                ; still characters left, use them
        push    hl
        ld      (YF23D),hl              ; transferaddress is I/O channel buffer
        ex      de,hl
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to FCB
        ld      hl,256
        call    A47B2                   ; random block read
        ld      e,l
        ld      d,h
        dec     h
        ld      a,l
        or      h                       ; have read 256 records ?
        pop     hl
        jr      z,A6861                 ; yep, then not at end of file
        push    hl
        add     hl,de
        ld      (hl),01AH               ; put a CTRL-Z at the end
        pop     hl
        xor     a
A6861:  ld      c,a
        ld      b,0
        add     hl,bc
        ld      a,(hl)                  ; get char
A6866:  ld      b,a
        sub     01AH
        sub     001H
        ld      a,b
        pop     hl
        inc     hl
        inc     hl
        inc     hl
        ld      (hl),0                  ; no backup char
        ret     nc                      ; no CTRL-Z, quit
        ld      (hl),a                  ; CTRL-Z as backup char, so it is always read again
        ret

;       Subroutine      putback for diskdevices (H.BAKU)
;       Inputs          
;       Outputs         ________________________

A6875:  ei
        push    hl
        ld      a,(YF348)
        add     a,a
        ld      hl,8
        jr      nc,A6882
        ld      l,8+8
A6882:  add     hl,sp
        ld      (hl),LOW NOSKCR
        inc     hl
        ld      (hl),HIGH NOSKCR        ; resume character putback
        pop     hl
        inc     hl
        inc     hl
        inc     hl
        ld      (hl),c
        ret

;       Subroutine      output to i/o channel (H.FILO)
;       Inputs          
;       Outputs         ________________________

A688E:  ld      ix,RETRTN
        ld      iy,00800H
        call    A6663                   ; take control from caller (move parameters on stack)
        ld      a,(hl)
        cp      2                       ; output mode ?
        jp      nz,A7199                ; nope, bad file mode error
        pop     af
        push    af
        call    A68A7                   ; write char to I/O channel
        jp      A669F                   ; quit

A68A7:  push    hl
        ld      bc,6
        add     hl,bc
        ld      c,(hl)                  ; position
        inc     (hl)                    ; update
        inc     hl
        inc     hl
        inc     hl                      ; to buffer
        add     hl,bc
        ld      (hl),a                  ; put char in buffer
        pop     hl
        ret     nz                      ; buffer not full, quit
A68B5:  push    hl
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to FCB
        ld      bc,4
        add     hl,bc
        ld      a,(hl)                  ; position
        inc     hl
        inc     hl
        inc     hl
        ld      (YF23D),hl              ; transferaddress
        and     a
        ld      l,a
        ld      h,b
        jr      nz,A68CB                ; not a complete buffer, only the used part
        inc     h                       ; 256
A68CB:  call    A718F                   ; random block write
        pop     hl
        ret

;       Subroutine      close i/o channel for diskdevices (H.NTFL)
;       Inputs          
;       Outputs         ________________________

A68D0:  ld      ix,RETRTN
        ld      iy,00400H
        call    A6663                   ; take control from caller (move parameters on stack)
        pop     hl
        ld      a,(hl)
        sub     2                       ; output mode ?
        jr      nz,A68F4                ; nope,
        push    hl
        ld      hl,FLBMEM
        cp      (hl)                    ; raw mode ?
        ld      (hl),a
        pop     hl
        jr      nz,A68F4                ; yep, skip CTRL-Z
        ld      (hl),4                  ; switch to random mode
        ld      a,01AH                  ; CTRL-Z
        call    A68A7                   ; write char to I/O channel
        call    nz,A68B5                ; buffer not empty, write remaining I/O channel buffer
A68F4:  push    hl
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to FCB
        call    A456F                   ; close fcb
        pop     hl
        push    hl
        ld      de,7
        add     hl,de
        ld      (hl),d                  ; clear i/o channel flags
        ld      l,d
        ld      h,d
        ld      (PTRFIL),hl             ; interpreter input/output device = keyboard/screen
        pop     hl
        inc     (hl)
        dec     (hl)
        ld      (hl),d
        pop     hl
        ret

;       Subroutine      Binary Save  (H.BINS)
;       Inputs          
;       Outputs         ________________________

A690E:  call    A7380                   ; take control from caller
        push    hl
        ld      ix,SCCPTR
        call    A731E                   ; convert linepointers to linenumbers
        ld      a,0FFH
        call    A69C7                   ; write byte to FCB (binairy basicfile id)
        ld      hl,(TXTTAB)
        ld      (YF23D),hl              ; transferaddress
        ex      de,hl
        ld      hl,(VARTAB)
        and     a
        sbc     hl,de			; size of BASIC program
        call    A718B			; random block write to FCB 0
        ld      (NLONLY),a              ; not loading basic program, close i/o channels when requested
        pop     hl
        ld      ix,CLSFIL
        jp      A731E                   ; close i/o channel

;       Subroutine      Binary Load  (H.BINL)
;       Inputs          
;       Outputs         ________________________

A6939:  ld      ix,M739A                ; quit loading & start (headloop/executing)
        ld      iy,00200H
        call    A6663                   ; take control from caller (move parameters on stack)
        pop     af			; MERGE statement ?
        jp      z,A7199			; yep, bad file mode error
        ld      ix,CLSALL
        call    A731E                   ; close all i/o channels
        ld      hl,(YF353)		; FCB for i/o channel 0
        push    hl
        call    A67FE                   ; setup FCB fields
        pop     hl
        push    hl
        ld      bc,16+3
        add     hl,bc
        ld      a,(hl)
        and     a			; file size >= 167772156 bytes ?
        jp      nz,A72FD		; yep, out of memory error
        dec     hl
        or      (hl)			; file size >= 65536 bytes ?
        jp      nz,A72FD		; yep, out of memory error
        dec     hl
        ld      d,(hl)
        dec     hl
        ld      e,(hl)			; file size
        ld      hl,(TXTTAB)
        add     hl,de			; does in fit in BASIC program area ?
        jp      c,A72FD			; nope, out of memory error
        ld      bc,00093H
        add     hl,bc			; does it leave room for stack space ?
        jp      c,A72FD			; nope, out of memory error
        sbc     hl,sp			; does it fit in memory ?
        jp      nc,A72FD		; nope, out of memory error
        ex      de,hl
        ex      (sp),hl
        ex      de,hl			; save file size, get FCB for i/o channel 0
        call    A69D0                   ; read byte from FCB
        ld      hl,(TXTTAB)
        ld      (YF23D),hl              ; transferaddresss
        pop     hl
        dec     hl			; file size -1
        call    A47B2                   ; random block read
        ld      de,(TXTTAB)
        add     hl,de
        ld      (VARTAB),hl		; setup start of BASIC variable area
        ld      ix,LINKER
        call    A731E                   ; recalculate linepointers
        ld      a,(FILNAM+0)
        and     a			; RUN after LOAD ?
        ret     nz			; nope, quit
        ld      (NLONLY),a              ; not loading basic program, close i/o channels when requested
        ld      hl,T69B9
        ld      de,BUF+10
        ld      bc,5
        push    de
        ldir
        pop     hl                      ; basicpointer
        ld      ix,NEWSTT
        jp      A731E                   ; continue in the execution loop

T69B9:  defb    ":",092H,0,0,0          ; :RUN

A69BE:  push    hl
        ld      a,l
        call    A69CB                   ; write byte to FCB
        pop     hl
        ld      a,h
        jr      A69CB                   ; write byte to FCB

A69C7:  ld      de,(YF353)		; FCB for i/o channel 0
A69CB:  ld      bc,A47BE                ; random block write
        jr      A69D3

A69D0:  ld      bc,A47B2                ; random block read
A69D3:  push    af
        ld      hl,1
        add     hl,sp
        ld      (YF23D),hl              ; transferaddress
        push    de
        call    A69E2
        pop     de
        pop     af
        ret

A69E2:  ld      hl,00001H
        push    bc
        ret

;       Subroutine      BSAVE for diskdevices
;       Inputs          
;       Outputs         ________________________

A69E7:  push    de
        call    A6B6A
        ld      (SAVENT),de
        push    de
        call    A6B6A
        ld      (SAVEND),de
        ex      (sp),hl
        ex      de,hl
        rst     020H
        jp      c,A7315
        ex      de,hl
        ex      (sp),hl
        call    A72DA                   ; at end of statement ?
        scf
        jr      z,A6A1B                 ; yep,
        call    A72D0
        defb    ","                     ; check for ","
        cp      "S"
        jr      nz,A6A13
        call    A72DB                   ; get basic character
        and     a
        jr      A6A1B

A6A13:  call    A6B6E
        ld      (SAVENT),de
        scf
A6A1B:  pop     bc
        jr      nc,A6A23
        inc     b
        dec     b
        jp      p,A7315
A6A23:  pop     de
        push    hl
        push    bc
        push    af
        xor     a                       ; i/o channel 0
        ld      e,2                     ; output mode
        ld      ix,OPNFIL
        call    A731E                   ; open i/o channel
        ld      a,0FEH
        call    A69C7                   ; write byte to FCB (bsave file id)
        pop     af
        pop     hl
        push    hl
        push    af
        call    A69BE                   ; write word to FCB
        ld      hl,(SAVEND)
        call    A69BE                   ; write word to FCB
        ld      hl,(SAVENT)
        call    A69BE                   ; write word to FCB
        pop     af
        pop     bc
        push    af
        ld      (YF23D),bc              ; transferaddress
        ld      hl,(SAVEND)
        and     a
        sbc     hl,bc
        inc     hl
        pop     af
        jr      nc,A6A6D
        call    A718B			; random block write to FCB 0
A6A5D:  ld      a,0FFH
        ld      (FLBMEM),a              ; raw mode
        xor     a
        ld      ix,CLSFIL
        call    A731E                   ; close i/o channel
        jp      A6EF8

A6A6D:  call    A7165
A6A70:  push    hl
        ld      de,(SAVENT)
        rst     020H
        push    af
        ld      c,l
        ld      b,h
        ld      hl,(SAVEND)
        push    hl
        add     hl,bc
        ld      (SAVEND),hl
        pop     hl
        ld      de,(YF23D)              ; transferaddress
        call    LDIRMV
        pop     af
        jr      nc,A6A9E
        pop     hl
        push    hl
        call    A718B			; random block write to FCB 0
        ld      hl,(SAVENT)
        pop     de
        and     a
        sbc     hl,de
        ld      (SAVENT),hl
        ex      de,hl
        jr      A6A70

A6A9E:  pop     hl
        ld      hl,(SAVENT)
        call    A718B			; random block write to FCB 0
        jr      A6A5D

;       Subroutine      BLOAD for diskdevices
;       Inputs          
;       Outputs         ________________________

A6AA7:  push    de
        xor     a
        ld      (RUNBNF),a              ; assume no autostart, no vram
        ld      c,a
        ld      b,a                     ; assume offset 0
        call    A72DA                   ; at end of statement ?
        jr      z,A6AD0                 ; yep,
        call    A72D0
        defb    ","                     ; check for ","
        cp      "R"                     ; autorun specified ?
        jr      z,A6ABF                 ; yep, set autorun
        cp      "S"                     ; vram specified ?
        jr      nz,A6ACB                ; nope, then it must be a offset
A6ABF:  ld      (RUNBNF),a
        call    A72DB                   ; get basic character
        jr      z,A6AD0                 ; end of statement, skip offset
        call    A72D0
        defb    ","                     ; check for ","
A6ACB:  call    A6B6E
        ld      b,d
        ld      c,e                     ; offset
A6AD0:  pop     de
        push    hl
        push    bc
        ld      a,0FFH
        ld      (FLBMEM),a              ; raw mode
        xor     a                       ; i/o channel 0
        ld      e,1                     ; input mode
        ld      ix,OPNFIL
        call    A731E                   ; open i/o channel
        ld      de,(YF353)		; FCB for i/o channel 0
        call    A69D0                   ; read byte from FCB
        cp      0FEH
        jp      nz,A7199		; nope, bad file mode error
        pop     bc
        call    A6B5C                   ; read word from FCB and add offset
        push    hl
        call    A6B5C                   ; read word from FCB and add offset
        push    hl
        call    A6B5C                   ; read word from FCB and add offset
        ld      (SAVENT),hl
        pop     hl
        pop     bc
        and     a
        sbc     hl,bc                   ; end address - start address
        inc     hl
        ld      (YF23D),bc              ; transferaddress
        ld      a,(RUNBNF)
        cp      "S"                     ; vram load ?
        jr      z,A6B1A                 ; yep,
        call    A47B2                   ; random block read
A6B11:  ld      ix,FINPRT
        call    A731E                   ; output back to screen
        pop     hl
        ret

A6B1A:  call    A7165
A6B1D:  push    hl
        ld      de,(SAVENT)
        rst     020H
        push    af
        ld      de,(YF353)		; FCB for i/o channel 0
        call    A47B2                   ; random block read
        pop     af
        pop     bc
        push    bc
        push    af
        ld      hl,(SAVEND)
        push    hl
        add     hl,bc
        ld      (SAVEND),hl
        pop     de
        ld      hl,(YF23D)              ; transferaddress
        pop     af
        jr      nc,A6B4E
        call    LDIRVM
        ld      hl,(SAVENT)
        pop     de
        and     a
        sbc     hl,de
        ld      (SAVENT),hl
        ex      de,hl
        jr      A6B1D

A6B4E:  pop     bc
        ld      bc,(SAVENT)
        call    LDIRVM
        xor     a
        ld      (RUNBNF),a
        jr      A6B11

A6B5C:  push    bc
        call    A69D0                   ; read byte from FCB
        push    af
        call    A69D0                   ; read byte from FCB
        ld      h,a
        pop     af
        ld      l,a
        pop     bc
        add     hl,bc
        ret

A6B6A:  call    A72D0
        defb    ","                     ; check for ","
A6B6E:  ld      ix,M6F0B
        jp      A731E                   ; evaluate address operand (BLOAD/SAVE)

;       Subroutine      DSKI$ function (H.DSKI)
;       Inputs          
;       Outputs         ________________________

A6B75:  call    A7380                   ; take control from caller
        call    A72DB                   ; get basic character
        call    A72D0
        defb    "("                     ; check for "("
        call    A6BC5
        call    A72D0
        defb    ")"                     ; check for ")"
        push    hl
        ld      hl,NULSTR
        ld      (DAC+2),hl
        pop     hl
        ld      a,3
        ld      (VALTYP),a
        and     a
        jr      A6BA1

;       Subroutine      DSKO statement (H.DSKO)
;       Inputs          
;       Outputs

A6B96:  call    A7380                   ; take control from caller
        call    A6BC5
        call    A72DA                   ; at end of statement ?
        ret     nz                      ; nope, quit
        scf
A6BA1:  push    af
        push    hl
        push    de
        ld      e,c
        call    A505D
        inc     a
        jp      z,A719C
        pop     de
        pop     hl
        pop     af
        push    hl
        ld      a,0FFH
        ld      (YF246),a               ; invalid dirsector buffer
        ld      a,(ix+0)                ; driveid
        ld      b,001H
        ld      c,(ix+1)                ; mediadescriptor
        ld      hl,(YF351)              ; temporary use dirsector buffer
        call    PHYDIO
        pop     hl
        ret

A6BC5:  ld      ix,GETBYT
        call    A731E                   ; evaluate byte operand
        push    de
        call    A72D0
        defb    ","                     ; check for ","
        ld      ix,GETUIN
        call    A731E                   ; evaluate address operand
        pop     bc
        ret

;       Subroutine      GET/PUT statement (H.DGET)
;       Inputs          
;       Outputs         ________________________

A6BDA:  ld      ix,RETRTN
        ld      iy,00400H
        call    A6663                   ; take control from caller (move parameters on stack)
        ld      a,(hl)
        cp      4                       ; random mode ?
        jp      nz,A7199                ; nope, bad file mode error
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; pointer to FCB
        ld      bc,9-2
        add     hl,bc			; i/o channel buffer
        ex      (sp),hl
        call    A72DA                   ; at end of statement ?
        jr      z,A6C29                 ; yep,
        push    de
        call    A72D0
        defb    ","                     ; check for ","
        ld      ix,FRMEVL
        call    A731E                   ; evaluate expression
        push    hl
        call    A6DFB
        ld      a,c
        or      b
        or      l
        or      h
        jp      z,A7315
        ld      a,c
        or      b
        dec     bc
        jr      nz,A6C16
        dec     hl
A6C16:  ex      de,hl
        pop     hl
        ex      (sp),hl
        push    hl
        push    de
        ld      de,00021H
        add     hl,de
        pop     de
        ld      (hl),c
        inc     hl
        ld      (hl),b
        inc     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        pop     de
        pop     hl
A6C29:  ex      (sp),hl
        ld      (YF23D),hl              ; transferaddress
        pop     hl
        pop     af
        push    hl
        and     a
        ld      hl,A7303
        ld      bc,A47B2                ; random block read
        jr      z,A6C3F
        ld      hl,A71A8
        ld      bc,A47BE                ; random block write
A6C3F:  push    hl
        call    A69E2
        and     a
        ret     nz
        pop     hl
        jp      A6EF8

;       Subroutine      FIELD statement (H.FIEL)
;       Inputs          
;       Outputs         ________________________

A6C49:  call    A7380                   ; take control from caller
        cp      "#"
        call    z,A72DB                 ; get basic character
        ld      ix,GETBYT
        call    A731E                   ; evaluate byte operand
        jp      z,A7318			; zero, syntax error
        push    hl
        ld      ix,FILIDX
        call    A731E                   ; get i/o channel pointer
        ld      e,l
        ld      d,h
        jp      z,A7300
        jp      c,A7315
        ld      a,(hl)
        cp      4			; random i/o mode ?
        jp      nz,A7199		; nope, bad file mode error
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        ld      bc,0000EH
        add     hl,bc
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        ld      (BUF+10),bc
        ld      hl,00000H
        ld      (BUF+12),hl
        ld      bc,00009H
        pop     hl
A6C8B:  ex      de,hl
        add     hl,bc
        ex      de,hl
        ld      a,(hl)
        cp      ","
        ret     nz
        push    de
        ld      ix,GTBYTC
        call    A731E                   ; skip basic char and evaluate byte operand
        push    af
        call    A72D0
        defb    "A"
        call    A72D0
        defb    "S"                     ; check for "AS"
        ld      ix,PTRGET
        call    A731E                   ; get address of variable
        ld      ix,GETYPR
        call    A731E                   ; GETYPR
        jp      nz,A72FA
        pop     af
        ex      (sp),hl
        push    de
        push    hl
        ld      hl,(BUF+12)
        ld      c,a
        ld      b,000H
        add     hl,bc
        ld      (BUF+12),hl
        ex      de,hl
        ld      hl,(BUF+10)
        rst     020H
        jp      c,A7306
        pop     de
        pop     hl
        ld      (hl),c
        inc     hl
        ld      (hl),e
        inc     hl
        ld      (hl),d
        ld      b,000H
        pop     hl
        jr      A6C8B

;       Subroutine      RSET statement (H.RSET)
;       Inputs          
;       Outputs         ________________________

A6CD6:  defb    0F6H

;       Subroutine      LSET statement (H.LSET)
;       Inputs          
;       Outputs         ________________________

A6CD7:  scf
        call    A7380                   ; take control from caller
        push    af
        ld      ix,PTRGET
        call    A731E                   ; get address of variable
        ld      ix,GETYPR
        call    A731E                   ; GETYPR
        jp      nz,A72FA
        push    de
        ld      ix,FRMEQL
        call    A731E                   ; evaluate =expression
        pop     bc
        ex      (sp),hl
        push    hl
        push    bc
        ld      ix,FRESTR
        call    A731E                   ; free temporary string
        ld      b,(hl)
        ex      (sp),hl
        ld      a,(hl)
        ld      c,a
        push    bc
        push    hl
        push    af
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        or      a
        jr      z,A6D6A
        ld      hl,(NULBUF)
        dec     hl
        rst     020H
        jr      c,A6D48
        ld      hl,(VARTAB)
        rst     020H
        jr      c,A6D48
        ld      e,c
        ld      d,000H
        ld      hl,(STKTOP)
        add     hl,de
        ex      de,hl
        ld      hl,(FRETOP)
        rst     020H
        jr      c,A6D7D
        pop     af
A6D2A:  ld      a,c
        ld      ix,GETSPA
        call    A731E                   ; allocate stringspace
        pop     hl
        pop     bc
        ex      (sp),hl
        push    de
        push    bc
        ld      ix,FRESTR
        call    A731E                   ; free temporary string
        pop     bc
        pop     de
        ex      (sp),hl
        push    bc
        push    hl
        inc     hl
        push    af
        ld      (hl),e
        inc     hl
        ld      (hl),d
A6D48:  pop     af
        pop     hl
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        pop     bc
        pop     hl
        inc     hl
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        ld      a,c
        cp      b
        jr      nc,A6D5A
        ld      b,a
A6D5A:  sub     b
        ld      c,a
        pop     af
        call    nc,A6D74
        inc     b
A6D61:  dec     b
        jr      z,A6D6F
        ld      a,(hl)
        ld      (de),a
        inc     hl
        inc     de
        jr      A6D61

A6D6A:  pop     bc
        pop     bc
        pop     bc
        pop     bc
        pop     bc
A6D6F:  call    c,A6D74
        pop     hl
        ret

A6D74:  ld      a,020H
        inc     c
A6D77:  dec     c
        ret     z
        ld      (de),a
        inc     de
        jr      A6D77

A6D7D:  pop     af
        pop     hl
        pop     bc
        ex      (sp),hl
        ex      de,hl
        jr      nz,A6DA9
        push    bc
        ld      a,b
        ld      ix,STRINI
        call    A731E                   ; allocate temporary string
        ld      de,TEMPST+30
        ld      hl,(TEMPPT)
        ld      (DAC+2),hl
        ld      a,3
        ld      (VALTYP),a
        call    VMOVE                   ; copy stringdescriptor
        ld      de,TEMPST+30+3
        rst     020H
        ld      (TEMPPT),hl
        jp      z,A72F7
        pop     bc
A6DA9:  ex      (sp),hl
        push    bc
        push    hl
        jp      A6D2A

;       Subroutine      MKI$ function (H.MKI$)
;       Inputs          
;       Outputs         ________________________

A6DAF:  ld      a,2
        defb    001H

;       Subroutine      MKS$ function (H.MKS$)
;       Inputs          
;       Outputs         ________________________

A6DB2:  ld      a,4
        defb    001H

;       Subroutine      MKD$ function (H.MKD$)
;       Inputs          
;       Outputs         ________________________

A6DB5:  ld      a,8
        call    A7380                   ; take control from caller
        push    af
        ld      ix,DOCNVF
        call    A731E                   ; convert DAC
        pop     af
        ld      ix,STRINI
        call    A731E                   ; allocate temporary string
        ld      hl,(DSCTMP+1)
        call    VMOVMF                  ; copy variable content from DAC
        jp      A61F1

; Unused code, patched code ??

        nop
        nop
        nop
        nop

;       Subroutine      CVI function (H.CVI)
;       Inputs          
;       Outputs         ________________________

A6DD7:  ld      a,2-1
        defb    001H

;       Subroutine      CVS function (H.CVS)
;       Inputs          
;       Outputs         ________________________

A6DDA:  ld      a,4-1
        defb    001H

;       Subroutine      CVD function (H.CVD)
;       Inputs          
;       Outputs         ________________________

A6DDD:  ld      a,8-1
        call    A7380                   ; take control from caller
        push    af
        ld      ix,FRESTR
        call    A731E                   ; free temporary string
        pop     af
        cp      (hl)
        jp      nc,A7315
        inc     a
        inc     hl
        ld      c,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,c
        ld      (VALTYP),a
        jp      VMOVFM                  ; copy variable content to DAC

A6DFB:  ld      ix,GETYPR
        call    A731E                   ; GETYPR
        ld      bc,(DAC+2)
        ld      hl,0
        ret     m
        jp      z,A72FA
        ld      hl,DAC
        ld      de,BUF+158
        ld      bc,8
        ldir
        ld      hl,T6E68
        ld      de,ARG
        ld      c,8
        ldir
        call    DECDIV                  ; dbl divide
        and     a                       ; double real flag
        call    INT                     ; dbl to integer
        ld      ix,M5432
        call    A731E                   ; convert address to integer
        push    de
        ex      de,hl
        ld      ix,FLTLIN
        call    A731E                   ; convert integer to sgn in DAC
        call    CONDS                   ; convert DAC from sgn to dbl
        ld      bc,06545H
        ld      de,06053H
        call    SGNMUL                  ; sgn multiply
        ld      hl,DAC
        ld      de,ARG
        ld      bc,8
        ldir
        ld      hl,BUF+158
        ld      de,DAC
        ld      c,8
        ldir
        call    DECSUB                  ; dbl subtract
        ld      ix,M5432
        call    A731E                   ; convert address to integer
        ld      c,e
        ld      b,d
        pop     hl
        ret

T6E68:  defb    045H,065H,053H,060H,000H,000H,000H,000H

;       Subroutine       (H.EOF)
;       Inputs          
;       Outputs         ________________________

A6E70:  call    A7380                   ; take control from caller
        push    hl
        call    A682A                   ; get char from I/O channel
        ld      hl,0
        jr      nc,A6E7D
        dec     hl
A6E7D:  push    af
        call    MAKINT                  ; integer to DAC
        pop     af
        pop     hl
        inc     hl
        inc     hl
        inc     hl
        ld      (hl),a
        ret

;       Subroutine      FILES/LFILES statement (H.FILE)
;       Inputs          
;       Outputs         ________________________

A6E88:  call    A7380                   ; take control from caller
        ld      d,000H
        jr      z,A6E95
        call    A6F80
        push    hl
        jr      A6E99

A6E95:  push    hl
        call    A6FFE
A6E99:  call    A6F63
        ld      a,(PRTFLG)
        and     a                       ; output to printer ?
        push    af
        call    A6645                   ; search file
        jp      z,A7312			; not found, file not found error
        ld      ix,CRDONZ
        call    A731E                   ; newline to OUTDO if not at start of line
A6EAE:  ld      hl,BUF+85
        ld      b,11
A6EB3:  ld      a,(hl)
        inc     hl
        rst     018H
        ld      a,b
        cp      004H
        jr      nz,A6EC3
        ld      a,(hl)
        cp      " "
        jr      z,A6EC2
        ld      a,"."
A6EC2:  rst     018H
A6EC3:  djnz    A6EB3
        call    CKCNTC
        pop     af                      ; output to printer ?
        push    af
        ld      a,(LINLEN)
        ld      b,a                     ; screenwidth
        ld      a,(TTYPOS)              ; screenpos
        jr      z,A6ED8                 ; nope, use screen
        ld      b,80                    ; printerwidth
        ld      a,(LPTPOS)              ; printerpos
A6ED8:  and     a                       ; at start of line ?
        jr      z,A6EEA                 ; yep, no newline
        add     a,00CH
        cp      b
        jr      nc,A6EE3
        ld      a," "
        rst     018H
A6EE3:  ld      ix,CRDO
        call    nc,A731E                ; yep, newline to OUTDO
A6EEA:  ld      de,BUF+10
        xor     a
        ld      (BUF+22),a
        call    A5006                   ; search for next
        inc     a
        jr      nz,A6EAE
        pop     af
A6EF8:  pop     hl
        ld      ix,FINPRT
        jp      A731E                   ; output back to screen and quit

;       Subroutine      KILL statement (H.KILL)
;       Inputs          
;       Outputs         ________________________

A6F00:  call    A7380                   ; take control from caller
        call    A6F96
        call    A72DA                   ; at end of statement ?
        ret     nz                      ; nope, quit
        call    A65EA                   ; is file already open in one of the I/O channels ?
        jp      z,A71A2
        call    A6F63
        push    hl
        ld      de,BUF+10
        call    A436C                   ; delete file
        and     a			; error ?
        jp      nz,A7312		; yep, file not found error
        pop     hl
        ret

;       Subroutine      NAME statement (H.NAME)
;       Inputs          
;       Outputs         ________________________

A6F20:  call    A7380                   ; take control from caller
        call    A6F96
        call    A65EA                   ; is file already open in one of the I/O channels ?
        jp      z,A71A2
        call    A6F63
        push    hl
        call    A6645                   ; search file
        jp      z,A7312			; not found, file not found error
        pop     hl
        call    A72D0
        defb    "A"
        call    A72D0
        defb    "S"                     ; check for "AS"
        call    A6F96
        ld      a,d
        ld      (BUF+26),a
        push    hl
        ld      hl,(BUF+10)
        and     a
        jr      z,A6F51
        cp      l
        jp      nz,A71B7
A6F51:  ld      de,BUF+27
        call    A665A
        ld      de,BUF+10
        call    A4392                   ; rename file
        and     a
        jp      nz,A71A5
        pop     hl
        ret

A6F63:  call    A65E3
        inc     a
        ld      (BUF+10),a
        push    hl
        push    de
        call    A6657
        pop     de
        pop     hl
        ret

A6F72:  ld      ix,FILEVL
        call    A731E                   ; evaluate filespecification
        ld      a,d
        cp      009H
        ret     c
        jp      A719C
A6F80:  call    A6F72
        push    hl
        ld      hl,FILNAM
        ld      b,00BH
A6F89:  ld      a,(hl)
        inc     hl
        cp      " "
        jr      nz,A6F9A
        djnz    A6F89
        call    A6FFE
        jr      A6F9A

A6F96:  call    A6F72
        push    hl
A6F9A:  ld      hl,FILNAM
        ld      b,008H
        call    A6FF5
        ld      b,003H
        call    A6FF5
        pop     hl
        defb    0F6H
A6FA9:  scf
        push    de
        push    hl
        ld      de,FILNAM
        push    de
        ld      b,11
A6FB2:  push    bc
        ld      a,(de)
        ld      hl,T6FCC
        ld      bc,0000DH
        jr      c,A6FBD
        dec     bc
A6FBD:  cpir
        jr      z,A6FF2
        pop     bc
        inc     de
        djnz    A6FB2
        pop     hl
        call    A6FD9
        pop     hl
        pop     de
        ret

T6FCC:  defb    '."/\[]:+=;,*?'

A6FD9:  ld      a," "
        cp      (hl)
        jr      z,A6FF2
        ld      b,007H
        call    A6FE5
        ld      b,003H
A6FE5:  inc     hl
        cp      (hl)
        jr      z,A6FEC
        djnz    A6FE5
        ret

A6FEC:  dec     b
        ret     z
        inc     hl
        cp      (hl)
        jr      z,A6FEC
A6FF2:  jp      A730F

A6FF5:  ld      a,(hl)
        cp      "*"
        jr      z,A7003
        inc     hl
        djnz    A6FF5
        ret

A6FFE:  ld      hl,FILNAM
        ld      b,00BH
A7003:  ld      (hl),"?"
        inc     hl
        djnz    A7003
        ret

;       Subroutine       (H.LOF)
;       Inputs          
;       Outputs         ________________________

A7009:  ld      bc,00010H
        defb    011H

;       Subroutine       (H.LOC)
;       Inputs          
;       Outputs         ________________________

A700D:  ld      bc,00021H
        call    A7380                   ; take control from caller
        push    bc
        ld      ix,CONINT
        call    A731E                   ; convert to byte
        ld      ix,FILIDX
        call    A731E                   ; get i/o channel pointer
        jp      c,A7315
        jp      z,A7300
        pop     bc
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        ex      de,hl
        add     hl,bc
        ld      c,(hl)
        inc     hl
        ld      b,(hl)
        inc     hl
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        ex      de,hl
        push    bc
        ld      ix,FLTLIN
        call    A731E                   ; convert integer to sgn in DAC
        ld      bc,06545H
        ld      de,06053H
        call    SGNMUL                  ; sgn multiply
        ld      hl,DAC
        ld      de,ARG
        ld      bc,8
        ldir
        pop     hl
        ld      ix,FLTLIN
        call    A731E                   ; convert integer to sgn in DAC
        call    CONDS                   ; convert DAC from sgn to dbl
        jp      DECADD                  ; dbl add

;       Subroutine      DSKF function (H.DSKF)
;       Inputs          
;       Outputs         ________________________

A7061:  call    A7380                   ; take control from caller
        ld      ix,CONINT
        call    A731E                   ; convert to byte
        ld      hl,YF347
        cp      (hl)
        jr      z,A7074
        jp      nc,A719C
A7074:  ld      e,a
        call    A505D
        jp      MAKINT                  ; integer to DAC

;       Subroutine      COPY statement (H.COPY)
;       Inputs          
;       Outputs         ________________________

A707B:  call    A7380                   ; take control from caller
        call    A6F80
        call    A65EA                   ; is file already open in one of the I/O channels ?
        jp      z,A71A2
        call    A6F63
        push    hl
        ld      a,(YF247)
        inc     a
        ld      (BUF+47),a
        ld      hl,BUF+11
        ld      de,BUF+48
        ld      bc,00024H
        ldir
        pop     hl
        call    A72DA                   ; at end of statement ?
        jr      z,A70C3                 ; yep,
        call    A72D0
        defb    0D9H                    ; check for TO token
        call    A6F80
        call    A65EA                   ; is file already open in one of the I/O channels ?
        jp      z,A71A2
        call    A65E3
        inc     a
        ld      (BUF+47),a
        push    hl
        ld      de,BUF+48
        call    A665A
        pop     hl
        call    A72DA                   ; at end of statement ?
        ret     nz                      ; nope, quit
A70C3:  push    hl
        call    A6645                   ; search file
        jp      z,A7312			; not found, file not found error
A70CA:  call    CKCNTC
        ld      bc,BUF+47
        ld      de,BUF+121
        ld      hl,BUF+84
        ld      a,00CH
A70D8:  push    af
        ld      a,(bc)
        cp      "?"
        jr      nz,A70DF
        ld      a,(hl)
A70DF:  ld      (de),a
        inc     bc
        inc     de
        inc     hl
        pop     af
        dec     a
        jr      nz,A70D8
        ld      hl,BUF+84
        ld      de,BUF+121
        ld      b,00CH
A70EF:  ld      a,(de)
        cp      (hl)
        jr      nz,A70FA
        inc     hl
        inc     de
        djnz    A70EF
        jp      A7315

A70FA:  call    A716C
        push    hl
        xor     a
        ld      (BUF+96),a
        ld      de,BUF+84
        call    A4462                   ; open fcb
        ld      de,BUF+121
        call    A461D                   ; create file
        and     a
        jp      nz,A71AB
        ld      l,a
        ld      h,a
        ld      (BUF+117),hl
        ld      (BUF+119),hl
        ld      (BUF+154),hl
        ld      (BUF+156),hl
        inc     hl
        ld      (BUF+98),hl
        ld      (BUF+135),hl
        pop     hl
A7128:  push    hl
        ld      de,BUF+84
        call    A47B2                   ; random block read
        ld      a,l
        or      h
        jr      z,A713C
        ld      de,BUF+121
        call    A718F                   ; random block write
        pop     hl
        jr      A7128

A713C:  pop     hl
        ld      hl,(BUF+104)
        ld      (BUF+141),hl
        ld      hl,(BUF+106)
        ld      (BUF+143),hl
        ld      de,BUF+121
        call    A456F                   ; close fcb
        ld      hl,BUF+84
        ld      (YF23D),hl              ; transferaddress
        ld      de,BUF+10
        xor     a
        ld      (BUF+22),a
        call    A5006                   ; search for next
        inc     a
        jp      nz,A70CA
        pop     hl
        ret

A7165:  ld      (SAVENT),hl
        ld      (SAVEND),bc
A716C:  ld      hl,0FE00H
        add     hl,sp
        jr      nc,A717F
        ld      de,(STREND)
        and     a
        sbc     hl,de
        jr      c,A717F
        ld      a,h
        and     a
        jr      nz,A7186
A717F:  ld      de,(NULBUF)
        ld      hl,256
A7186:  ld      (YF23D),de              ; transferaddress
        ret

A718B:  ld      de,(YF353)		; FCB for i/o channel 0
A718F:  call    A47BE                   ; random block write
        and     a
        ret     z
        jr      A71A8

A7196:  ld      e,03CH
        defb    001H
A7199:  ld      e,03DH
        defb    001H
A719C:  ld      e,03EH
        defb    001H
        ld      e,03FH
        defb    001H
A71A2:  ld      e,040H
        defb    001H
A71A5:  ld      e,041H
        defb    001H
A71A8:  ld      e,042H
        defb    001H
A71AB:  ld      e,043H
        defb    001H
A71AE:  ld      e,044H
        defb    001H
A71B1:  ld      e,045H
        defb    001H
A71B4:  ld      e,046H
        defb    001H
A71B7:  ld      e,047H
        ld      bc,00000H
        xor     a
        ld      (NLONLY),a              ; not loading basic program, close i/o channels when requested
        ld      (FLBMEM),a              ; ascii mode
        push    de
        ld      ix,CLSFIL
        call    A731E                   ; close i/o channel
        pop     de
        ld      ix,ERROR                ; BASIC error
        jp      A731E

;       Subroutine      expand errormessages (H.ERRP)
;       Inputs          
;       Outputs         ________________________

A71D3:  ld      a,e
        cp      03CH                    ; normal BASIC error ?
        ret     c                       ; yep, quit
        cp      048H                    ; DiskBASIC error ?
        ret     nc                      ; nope, quit
        sub     03BH
        ld      b,a                     ; 1 based offset
        ld      hl,T71F5
A71E0:  ld      a,(hl)
        and     a
        inc     hl
        jr      nz,A71E0
        djnz    A71E0                   ; next errormessage
        dec     hl                      ; include trailing zero
        ld      de,BUF+166
        push    de
        ld      bc,22
        ldir                            ; copy errormessage to temporary place
        ld      e,1                     ; erroroffset 1
        pop     hl
        ret

T71F5:  defb    0
        defb    "Bad FAT",0
        defb    "Bad file mode",0
        defb    "Bad drive name",0
        defb    "Bad sector number",0
        defb    "File still open",0
        defb    "File already exists",0
        defb    "Disk full",0
        defb    "Too many files",0
        defb    "Disk write protected",0
        defb    "Disk I/O error",0
        defb    "Disk offline",0
        defb    "Rename across disk",0


T72AE:  defw    A72B0                   ; pointer to the default diskerror handler for DiskBASIC

;       Subroutine      diskerror handler for DiskBASIC
;       Inputs          ________________________
;       Outputs         ________________________

A72B0:  bit     7,c                     ; FAT error ?
        jp      nz,A7196                ; yep, bad fat error
        res     0,c
        ld      b,0
        ld      hl,T72C2
        add     hl,bc
A72BD:  ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        jp      (hl)

T72C2:  defw    A71AE                   ; Write Protect error, disk write protect error
        defw    A71B4                   ; Not Ready error, disk offline error
        defw    A71B1                   ; Data/CRC error, disk i/o error
        defw    A71B1                   ; Seek error, disk i/o error
        defw    A71B1                   ; Record not found error, disk i/o error
        defw    A71B1                   ; Write fault error, disk i/o error
        defw    A71B1                   ; Other error, disk i/o error

;       Subroutine      check for BASIC character
;       Inputs          HL = BASIC pointer
;       Outputs         ________________________

A72D0:  call    A72DA
        ex      (sp),hl
        cp      (hl)
        jr      nz,A7318		; syntax error
        inc     hl
        ex      (sp),hl
        inc     hl
A72DA:  dec     hl
A72DB:  ld      ix,CHRGTR
        jr      A731E                   ; continue in CHRGTR

; unused code, patched ??

        nop
        nop
        nop

A72E4:  ld      a,0C9H
A72E6:  ld      (H.LOPD+0),a
        ld      de,(HIMEM)
        ld      (YF349),de
        ret

T72F2:  rst     030H
        defb    000H
        defw    A72E4
        ret

A72F7:  ld      e,010H
        defb    001H

A72FA:  ld      e,00DH
        defb    001H

A72FD:  ld      e,007H
        defb    001H

A7300:  ld      e,03BH
        defb    001H

A7303:  ld      e,037H
        defb    001H

A7306:  ld      e,032H
        defb    001H

A7309:  ld      e,036H
        defb    001H

A730C:  ld      e,034H
        defb    001H

A730F:  ld      e,038H
        defb    001H

A7312:  ld      e,035H
        defb    001H

A7315:  ld      e,005H
        defb    001H

A7318:  ld      e,002H
        ld      ix,ERROR                ; BASIC error
A731E:  call    CALBAS
        ei
        ret

;       Subroutine      devicename parser (H.PARD)
;       Inputs          
;       Outputs         ________________________

A7323:  ei
        push    hl
        push    de
        ld      a,(YF348)
        add     a,a
        ld      hl,16
        jr      nc,A7331
        ld      l,16+8
A7331:  add     hl,sp
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        push    hl
        ld      hl,BSAVE+3
        rst     020H                    ; called from BSAVE ?
        pop     hl
        jr      z,A734A                 ; yep, adjust returnaddress for DiskBASIC BSAVE
        push    hl
        ld      hl,BLOAD+3
        rst     020H                    ; called from BLOAD ?
        pop     hl
        jr      nz,A7350                ; nope, do not adjust returnaddress
        ld      de,XF377
        jr      A734D                   ; yep, adjust returnaddress for DiskBASIC BLOAD

A734A:  ld      de,XF37A
A734D:  ld      (hl),d
        dec     hl
        ld      (hl),e
A7350:  pop     de
        pop     hl
        ld      a,e
        cp      2                       ; filespecification length <2 ?
        ret     c                       ; yep, no device specified
        ld      a,(hl)
        cp      ":"                     ; first character a ":" ?
        jr      z,A730F                 ; yep,
        inc     hl
        ld      a,(hl)
        cp      ":"                     ; second character a ":" ?
        dec     hl
        ret     nz                      ; nope, no diskdevice specifier
        call    A7380                   ; take control from caller
        ld      a,(hl)
        and     0DFH                    ; upcase
        sub     040H                    ; to drivenumber/deviceid for disk (1...) 
        push    hl
        ld      hl,YF347
        cp      (hl)                    ; valid drive ?
        pop     hl
        jr      z,A7374
        jp      nc,A719C                ; nope,
A7374:  inc     hl
        inc     hl
        dec     e
        dec     e
        push    de
        inc     e                       ; flag Zx reset
        pop     de
        ret

;       Subroutine      no device specified (H.NODE)
;       Inputs          
;       Outputs         ________________________

A737C:  ei
        ld      a,0                     ; deviceid for default drive
        ret

;       Subroutine      take control from caller
;       Inputs          
;       Outputs         ________________________

;       This is what the stack looks like at entry:
;
;       prim    exp
;       +0      +0      returnaddress A7380 caller
;       +2      +2      callf BIOS registers
;       +6      +14     returnaddress CALLF caller
;       +8      +16     returnaddress hook caller

A7380:  ei
        push    hl
        push    af
        ld      a,(YF348)
        add     a,a
        ld      hl,12
        jr      nc,A738E
        ld      l,12+8
A738E:  add     hl,sp
        ld      (hl),LOW RETRTN
        inc     hl
        ld      (hl),HIGH RETRTN
        pop     af
        pop     hl
        ret

;       Subroutine      BDOS 09 (output string)
;       Inputs          DE = address of string
;       Outputs         ________________________
;       Remark          is copied to 0F1C9H

A7397:  call    XF36B                   ; enable ram on page 1
        ld      a,(de)
        call    XF368                   ; enable master diskrom
        inc     de
        cp      '$'
        ret     z                       ; end of string, quit
        call    A53A8                   ; console output
        jr      A7397                   ; next

;       Subroutine      XFER (transfer)
;       Inputs          HL = source address, DE = destition address, BC = size
;       Outputs         ________________________
;       Remark          is copied to 0F1D9H

        ;call    XF36B                   ; enable ram on page 1
        nop
        nop
        nop
        ldir                            ; transfer
        ;call    XF368                   ; enable master diskrom
        nop
        nop
        nop
        ret

;       Subroutine      Warm Boot
;       Inputs          ________________________
;       Outputs         ________________________
;       Remark          is copied to 0F1E2H

        call    XF36B                   ; enable ram on page 1
        jp      0                       ; WBOOT

;       Subroutine      start handler in DOS memory
;       Inputs          HL = address of pointer
;       Outputs         ________________________
;       Remark          is copied to 0F1E8H

        ld      de,XF1D9+5
        push    de                      ; on return, enable master diskrom
        ld      e,(hl)
        inc     hl
        ld      d,(hl)                  ; get pointer
        ex      de,hl
        call    XF36B                   ; enable ram on page 1
        jp      (hl)                    ; start it

;       Subroutine      validate FCB filename
;       Inputs          HL = address of pointer
;       Outputs         ________________________
;       Remark          is copied to 0F1F4H

        jp      A5604

;       Data            table with reserved filenames (devicenames)
;       Remark          is copied to 0F1F7H

        defb    "PRN "
        defb    "LST "
        defb    "NUL "
        defb    "AUX "
        defb    "CON "

;       Data            fake direntry for devices
;       Remark          is copied to 0F20BH

        defb    "           "
        defb    10000000b
        defs    10
        defw    0
        defw    0
        defw    0
        defw    0,0

        defb    31,28,31,30,31,30,31,31,30,31,30,31

; DRIVER section starts here

DSKDRV:

        INCLUDE driver.asm

        DEFS    08000H-$,0
ENDADR:

        end


