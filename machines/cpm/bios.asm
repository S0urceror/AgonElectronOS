;   .ASSUME ADL=0

; BLOAD header
    db 0x0fe
    dw BEGIN, ENDADR, START_BASIC

    .ORG 0000h
BEGIN:
START_BASIC:

BOOT:       JP 0xF200    ; 0x0000 - BIOS JUMP COLD BOOT, will be replaced with WARM BOOT vector
IOBYTE:     DB 0         ; 0x0003 - real/virtual
DEFDSKUSR:  DB 0         ; 0x0004 - default drive and user
BDOS_ENTRY: JP 0xE400+6  ; 0x0005 - BDOS entry point - FBASE

ENDADR:
