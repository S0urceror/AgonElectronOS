	INCLUDE "ez80f92.inc"
	INCLUDE	"macros.inc"
	.ASSUME ADL=1

	;DEFINE .STARTUP, SPACE = ROM
    SEGMENT CODE ;.STARTUP

	XDEF	electron_os_api
	XDEF	electron_os_inout
	XDEF	_machine_vblank_handler

	XREF	_machine_read_write_disk
    XREF 	_machine_warm_boot
	XREF	_machine_vsync_address
	XREF	_machine_vsync_register
	XREF    _machine_vsync_running
	XREF	_eos_msx_keyboard_scanline
	;
	XREF	SLOT_REGISTER
	XREF	call_address_ix
	XREF    _prev_mbase
	XREF	_callSM
	;
	XREF	uart0_send_fifo_add
	XREF	uart0_recv_fifo_get
	XREF	uart0_recv
	XREF	uart0_send
	;
	XREF    _machine_fopen
	XREF	_machine_fclose
	XREF	_machine_fgetc
	XREF	_machine_feof
	XREF    _machine_fputc
	XREF	_machine_putc

JUMPER EQU 0f3fch ;Work area of the data recorder. (Until MSX2+) 

; ElectronOS runs from RAM at 0x00000, personality RAM starts from 0x10000
.ifdef _DEBUG
SLOT_0_64K_SEGMENT EQU 01h
.endif
; ElectronOS runs from ROM at 0x00000, personality RAM starts from 0x50000
.ifdef NDEBUG
SLOT_0_64K_SEGMENT EQU 05h
.endif

; API's in low 64kb so 16-bit range
; call API specified by IX
electron_os_api:
	push af
    push bc
	; let ix point to address in jumptable
    ld bc, jumptable
    add ix,bc
	; read contents of jumptable
	ld c, (ix+0)
	ld b, (ix+1)
	; put in ix
	push bc
	pop ix
	; restore bc+af
    pop bc
	pop af
	; jump to address specified in jumptable
    jp (ix)

jumptable:
    DW eos_machine_read_write_disk				;0x0000
    DW eos_machine_reload_warmboot_images		;0x0002
    DW eos_msx_machine_slotregister				;0x0004
    DW eos_msx_machine_rdslt					;0x0006
    DW eos_msx_machine_wrslt					;0x0008
    DW eos_msx_machine_calslt					;0x000a
    DW eos_msx_machine_enaslt					;0x000c
	DW eos_msx_machine_setvblankaddress			;0x000e
	DW eos_msx_machine_getvdpstatus				;0x0010
	DW eos_msx_machine_getscanline				;0x0012
	DW eos_msx_machine_ldirvm					;0x0014
	DW eos_msx_machine_ldirmv					;0x0016
	DW eos_msx_machine_fillvrm					;0x0018
	DW eos_machine_fopen						;0x001a
	DW eos_machine_fclose						;0x001c
	DW eos_machine_fputc						;0x001e
	DW eos_machine_fgetc						;0x0020
	DW eos_machine_feof							;0x0022
	DW eos_machine_opendir						;0x0024
	DW eos_machine_readdir						;0x0026
	DW eos_machine_closedir						;0x0028
	DW eos_machine_chdir						;0x002a

; read/write a sector from/to the disk
; 
eos_machine_read_write_disk:
	; push all registers to the stack to pass to function
	push hl
	push de
	push bc
	push af
	ld a, mb
	push af
	call _machine_read_write_disk ; HL contains new AF at return
	push hl
	pop af
	; restore the other registers
	pop hl ; old mb contents is not modified
	pop hl ; old af contents can be thrown away
	pop bc
	pop de
	pop hl
    ret

eos_machine_reload_warmboot_images:
	push hl
	push de
	push bc
	push af
	call _machine_warm_boot
	pop af
	pop bc
	pop de
	pop hl
    ret
	
eos_msx_machine_slotregister:
	jr c, _slotregister_write
_slotregister_read:
	ld a, (SLOT_REGISTER)
	ret
_slotregister_write:
	ld (SLOT_REGISTER),a
	ret

; IN  - A  slot (ExxxSSPP)
;       HL address to read in 16-bit space
; OUT - result in A
eos_msx_machine_rdslt:
	; assumptions
	; we have no expanded slots
	; we only do primary slots
	; ROM, slot 0, in 0x10000-0x1ffff , other slots RAM 0x20000 - 0x7ffff
	push ix
	push hl
	; add start ram slot
	add a, SLOT_0_64K_SEGMENT
	; push hl to stack which gives:
	; SP   - L
	; SP+1 - H
	; SP+2 - U
	push hl
	ld ix, 0
	add ix, sp
	; modify hl on stack to point to right slot
	ld (ix+2),a
	pop hl
	ld a, (hl)
	pop hl
	pop ix
	ret
	
eos_msx_machine_wrslt:
	and a
	ret z ; do not attempt to write ROM section
	push ix
	push hl
	; add start ram slot
	add a, SLOT_0_64K_SEGMENT
	; push hl to stack which gives:
	; SP   - L
	; SP+1 - H
	; SP+2 - U
	push hl
	ld ix, 0
	add ix, sp
	; modify hl on stack to point to right slot
	ld (ix+2),a
	pop hl
	ld (hl),e
	pop hl
	pop ix
	ret

;Function : Executes inter-slot call.
;Input    : IY - High byte with slot ID, see RDSLT
;           IX - The address that will be called
;Remark   : Variables can never be given in alternative registers or IX and IY
eos_msx_machine_calslt:
	push af
	pop ix
	; store mb
	ld a, mb
	ld (_prev_mbase),a
	; store registers we are going to change
	push ix ; was AF
	push de
	;
	ld a, iyh
	add a,SLOT_0_64K_SEGMENT
	ld mb, a
	out0 (RAM_ADDR_U), a ; remap internal ram to upper 4k of selected 64kB
	;
	; push de to stack which gives:
	; SP   - E
	; SP+1 - D
	; SP+2 - U
	push de
	ld iy, 0
	add iy, sp
	; modify DE on stack to point to JUMPER address in right slot
	; A contains slot/mb
	ld (iy+2),a
	ld a, HIGH JUMPER
	ld (iy+1),a
	ld a, LOW JUMPER
	ld (iy+0),a
	; get it back
	pop de
	; change JUMPER address to call address
	ld ix, call_address_ix
	ld a, 0cdh	 ; CALL
	ld (de),a
	inc de
	ld a, (ix+0) ; LOW IX
	ld (de),a
	inc de
	ld a, (ix+1) ; HIGH IX
	ld (de),a
	inc de
	ld a, 049h   ; LIS
	ld (de),a
	inc de
	ld a, 0c9h   ; RET
	ld (de),a
	;
	pop de
	pop af
	;
	call.is JUMPER ; JUMPER routine
	;
	; restore mb
	push af
	ld a, (_prev_mbase)
	ld mb, a
	out0 (RAM_ADDR_U), a ; remap internal ram to upper 4k of selected 64kB
	pop af
	ret

eos_msx_machine_enaslt:
	; we don't do anything yet...

	; we could check calling stack
	; scrub the stack
	; switch MB
	; jump to return address in selected slot in simplified ROM 0/RAM 1 system
	ret

eos_msx_machine_setvblankaddress:
	; clear running flag
	push af
	xor a
	ld (_machine_vsync_running),a
	pop af
	; set address
	ld (_machine_vsync_address),hl
	ret

eos_msx_machine_getvdpstatus:
	ld a, (_machine_vsync_register)
	push af
	xor a
	ld (_machine_vsync_register),a
	pop af
	ret
	
_machine_vblank_handler:
	push    hl
	push    de
	push    bc
	push    af
	exx
	ex      af,af'
	push    hl
	push    de
	push    bc
	push    af
	push    iy
	push    ix
	; reset GPIO edge-trigger
	SET_GPIO 	PB_DR, 2		; Need to set this to 2 for the interrupt to work correctly
	; signal PORTC bit 0
	; IN0	A,(PC_DR)
	; CPL
	; OUT0 (PC_DR),A
	; check if another interrupt routine is still running, prevent from running again
	ld a, (_machine_vsync_running)
	and 1
	jr nz,_exit_vblank_handler
	inc a
	ld (_machine_vsync_running),a
	; is vsync address set? if not skip
	ld hl,(_machine_vsync_address)
	ld a, l
	or h
	jr z, _exit_vblank_handler	
	; set vsync status for fast RDVDP
	ld a, 080h
	ld (_machine_vsync_register), a
	; push de to stack which gives:
	; SP   - E
	; SP+1 - D
	; SP+2 - U
	push de
	ld iy, 0
	add iy, sp
	; modify DE on stack to point to JUMPER address in right slot
	ld a, mb
	ld (iy+2),a
	ld a, HIGH JUMPER
	ld (iy+1),a
	ld a, LOW JUMPER
	ld (iy+0),a
	; get it back
	pop de
	; change JUMPER address to call address
	ld a, 0cdh	 ; CALL
	ld (de),a
	inc de
	ld a, l
	ld (de),a
	inc de
	ld a, h
	ld (de),a
	inc de
	ld a, 049h   ; LIS
	ld (de),a
	inc de
	ld a, 0c9h   ; RET
	ld (de),a
	; call the self-modified JUMPER routine in short Z80 mode
	call.is JUMPER ; JUMPER routine	
	; clear flag to prevent re-entry
	xor a
	ld (_machine_vsync_running),a
	; we're done, let's wrap up
_exit_vblank_handler:
	pop     ix
	pop     iy
	pop     af
	pop     bc
	pop     de
	pop     hl
	ex      af,af'
	exx
	pop     af
	pop     bc
	pop     de
	pop     hl
	ei
	RETI.L

; character to be printed passed via the stack
_machine_putc:
	push ix
	push iy
	ld ix,0
	ld iy,0
	add ix,sp
	ld a, (ix+9) ; character to be printed
	ld IYh, 0
	ld IX, 0a2h
	ld (call_address_ix),ix
	call eos_msx_machine_calslt
	pop iy
	pop ix
	ret

; IN/OUT to port specified in IYl
; OUT value in IYh
; Cy = write, No Cy = read
electron_os_inout:
	push af ; A
	ld a, iyl
	cp 0a8h
	jr z, _electron_os_inout_slotregister
	cp 0ffh ; if port number is ffh then it is passed in register C
	jr nz, _electron_os_inout_next_1
	ld iyl, c
_electron_os_inout_next_1:	
	pop af  ; A
	push af	; A
	; send command to HAL
	ld a, 080h ; 0b10000000 ; send
	; check OUT=C or IN=NC
	jr c, _electron_os_inout_next_2
	; IN mode
	or 001h ; 0b00000001 ; recv
	; switch off uart0 receive interrupts
	; we go direct mode
	push af ; B
	in0 a, (UART0_IER)
	and 0feh ;0b11111110
    out0 (UART0_IER),a
	pop af  ; B
_electron_os_inout_next_2:
	; send command 0x80 (out) or 0x81 (in)
	call uart0_send
	; send port number
	ld a, iyl
	call uart0_send
	pop af  ; A
	; check OUT=C or IN=NC
	jr nc, _electron_os_inout_recv_1
	; send last byte, value
	ld a, iyh
	call uart0_send
	ret
_electron_os_inout_recv_1:
	call uart0_recv
	; switch uart0 receive interrupts on again
	push af
	in0 a, (UART0_IER)
	or 001h ;0b00000001
    out0 (UART0_IER),a
	pop af
	; value returned in A
	ret
_electron_os_inout_slotregister:
	pop af
	jp eos_msx_machine_slotregister

; This routine is routinely called to get the current state of keyboard scanlines
; In:  A = scanline row
; Out: A = bits of scanline
;
; When A=0 we are going to get the status of all scanlines in one go
; subsequent requests are handled from the buffered status
eos_msx_machine_getscanline:
	push bc
	push hl
	and a
	call z, eos_msx_getallscanlines
	ld bc,0
	ld c,a
	ld hl,_eos_msx_keyboard_scanline
	add hl,bc
	ld a, (hl)
	pop hl
	pop bc
	ret

eos_msx_getallscanlines:
	push af
	di
	ld b, 11   ; 11 scanlines
    ld a, 083h ; input repeat
	call uart0_send
    ld a, 0a9h
    call uart0_send
    ld a, 0
    call uart0_send
    ld a, b
    call uart0_send
    ; switch off uart0 receive interrupts
	; we go direct mode
	in0 a,(UART0_IER)
	and 0feh ;0b11111110
    out0 (UART0_IER),a
    ;
	ld hl, _eos_msx_keyboard_scanline
getallnext:
    ; while characters in fifo => process
    in0 a,(UART0_LSR)
    bit 0,a ; check receive data ready, 1 = character(s) in FIFO/RBR, 0 = empty
    jr z, getallnext
    in0 a,(UART0_RBR)
    ld (hl),a
    inc hl
    djnz getallnext  
    ; switch on uart0 receive interrupts
	in0 a,(UART0_IER)
	or 001h ;0b00000001
    out0 (UART0_IER),a
	ei
	pop af
	ret

;Address  : #0050
;Function : Enable VDP to read
;Input    : HL - For VRAM-address
;Registers: AF
SETRD:
	;out     (099H),a
	ld a, 080h ; output
	call uart0_send
	ld a, 099h
	call uart0_send
	ld a, l
	call uart0_send
	;out     (099H),a
	ld a, 080h ; output
	call uart0_send
	ld a, 099h
	call uart0_send
	ld a, h
	and     03FH		; high bit 0 = set read address
	call uart0_send
    ret

;Address  : #0053
;Function : Enable VDP to write
;Input    : HL - Address
;Registers: AF
SETWRT:
	;out     (099H),a
	ld a, 080h ; output
	call uart0_send
	ld a, 099h
	call uart0_send
	ld a, l
	call uart0_send
	;out     (099H),a
	ld a, 080h ; output
	call uart0_send
	ld a, 099h
	call uart0_send
	ld a, h
	and     03FH
	or      040H		; high bit 1 = set write address
	call uart0_send
    ret

; Function : Block transfer to VRAM from memory
; Input    : BC - Block length
;            DE - Start address of VRAM
;            HL - Start address of memory
; Registers: All
eos_msx_machine_ldirvm:
	di
    ex de,hl
    call SETWRT
    ld a, 082h ; output repeat
    call uart0_send
	ld a, 098h
    call uart0_send
    ld a, b
    call uart0_send
    ld a, c
    call uart0_send
	;
	push ix ; save IX
	; push de to stack which gives:
	; SP   - E
	; SP+1 - D
	; SP+2 - U
	push de
	ld ix, 0
	add ix, sp
	; modify DE on stack to point to right 64kB segment
	ld a, mb
	ld (ix+2),a
	pop de ; DE updated with right 24-bits address
LDIRVMN:
    ld a, (de)
    call uart0_send
    inc de
    dec bc
    ld a, b
    or c
    jr nz, LDIRVMN
	pop ix ; restore IX
    ei
    ret
; Function : Block transfer to memory from VRAM
; Input    : BC - Block length
;            DE - Start address of memory
;            HL - Start address of VRAM
; Registers: All
eos_msx_machine_ldirmv:
    di
    call SETRD
    ld a, 083h ; input repeat
	call uart0_send
    ld a, 098h
    call uart0_send
    ld a, b
    call uart0_send
    ld a, c
    call uart0_send
    ; switch off uart0 receive interrupts
	; we go direct mode
	in0 a,(UART0_IER)
	and 0feh ;0b11111110
    out0 (UART0_IER),a
    ;
	push ix ; save IX
	; push DE 16-bit address to stack which gives:
	; SP   - E
	; SP+1 - D
	; SP+2 - U
	push de
	ld ix, 0
	add ix, sp
	; modify DE on stack to point to right 64kB segment
	ld a, mb
	ld (ix+2),a
	pop de ; DE updated with right 24-bits address	
LDIRMVN:
    ; while characters in fifo => process
    in0 a,(UART0_LSR)
    bit 0,a ; check receive data ready, 1 = character(s) in FIFO/RBR, 0 = empty
    jr	z, LDIRMVN
    in0 a,(UART0_RBR)
    ld (de),a
    inc de
    dec bc
    ld a, b
    or c
    jr nz, LDIRMVN  
    ; switch on uart0 receive interrupts
	in0 a,(UART0_IER)
	or 001h ;0b00000001
    out0 (UART0_IER),a
    ;  
	pop ix
    ei
    ret
; Function : Fill VRAM with value
; Input    : A  - Data byte
;            BC - Length of the area to be written
;            HL - Start address
; Registers: AF, BC
eos_msx_machine_fillvrm:
FILVRM_FAST:
	di
    push af
    call SETWRT
    ld a, 084h ; output fill
    call uart0_send
    ld a, 098h
    call uart0_send
    ld a, b
    call uart0_send
    ld a, c
    call uart0_send
    POP     AF
	call uart0_send
    ei
    RET

eos_machine_fopen:
	; Called with:
	; reg A = channelnumber, reg DE = filename, reg C = openmode
	;
	push ix ; save ix
	push bc ; 3 - openmode
	ex af, af'
	; modify de on stack to point to filename in right slot
	ld a, mb
	push de ; 2 - filename
	ld ix, 0
	add ix, sp
	ld (ix+2),a
	;
	ex af, af'
	ld c, a
	push bc ; 1 - channel
	call _machine_fopen ; char machine_fopen (UINT8 channel,char* filename,UINT8 openmode)
	pop de ; 1 - was BC
	pop de ; 2 - was DE
	pop bc ; 3 - was BC
	pop ix ; restore ix
	ret

eos_machine_fclose:
	; Called with:
	; reg A = channelnumber
	ld bc, 0
	ld c,a
	push bc ; 1 - channel number
	call _machine_fclose ; char machine_fclose (UINT8 channel)
	pop bc ; 1 - dummy
	ret

eos_machine_fputc:
	; Called with
	; reg A = channelnumber, reg L = character being output
	push de ; store de
	ld de,0
	ld e,l
	push de ; 2 - character
	ld de, 0
	ld e, a
	push de ; 1 - channelnumber
	call _machine_fputc ; char machine_fputc (UINT8 channel,UINT8 character)
	pop de
	pop de
	pop de ; restore de
	ret

eos_machine_fgetc:
	; Called with:
	; reg A = channelnumber, reg DE = pointer to buffer to receive character
	push ix ; save ix
	ex af, af'
	; modify de on stack to point to filename in right slot
	ld a, mb
	push de ; 2 - buffer
	ld ix, 0
	add ix, sp
	ld (ix+2),a
	;
	ex af, af'
	ld de, 0
	ld e, a
	push de ; 1 - channelnumber
	call _machine_fgetc ; char machine_fgetc (UINT8 channel, char* character)
	pop de ; 1 - dummy
	pop de ; 2 - was DE
	pop ix ; restore ix
	ret

eos_machine_feof:
	; Called with:
	; reg A = channelnumber
	ld bc, 0
	ld c, a
	push bc
	call _machine_feof
	pop bc ; dummy
	ret

eos_machine_opendir:
eos_machine_readdir:
eos_machine_closedir:
eos_machine_chdir:
	ld a, 1
	RET