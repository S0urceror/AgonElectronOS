	INCLUDE "ez80f92.inc"
	INCLUDE	"macros.inc"
	.ASSUME ADL=1

	;DEFINE .STARTUP, SPACE = ROM
    SEGMENT CODE ;.STARTUP

	XDEF	electron_os_api
	XDEF	electron_os_inout
	XDEF	_machine_vblank_handler
	XDEF	_vdp_test
	; XDEF	checkEIstate

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

JUMPER EQU 0f3fch ;Work area of the data recorder. (Until MSX2+) 
SLOT_0_64K_SEGMENT EQU 01h

; API's in low 64kb so 16-bit range
; call API specified by IX
electron_os_api:
	push af
    push bc
	; let ix point to address in jumptable
    ld bc, jumptable
    add ix,bc
	; read contens of jumptable
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
	DW eos_msx_machine_writeport				;0x0014

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

; 	IF $ < 100H
;         ERROR "Must be at address >= 100H"
;     ENDIF
; ; Workaround for LD A,I / LD A,R lying to us if an interrupt occurs during the
; ; instruction. We detect this by examining if (sp - 1) was overwritten.
; ; f: pe <- interrupts enabled, po <- interrupts disabled
; ; Modifies: af
; checkEIstate:
;     xor a
;     push af  ; set (sp - 1) to 0
;     pop af
;     ld a,i	; check IFF2, 1 is EI, 0 is DI
;     ret pe  ; interrupts enabled? 1 is pe is EI, 0 is po is DI
; 	; interrupts disabled (DI)? let's check to be sure
;     dec sp
; 	dec sp
;     dec sp  ; check whether the Z80 lied about ints being disabled
;     pop af  ; (sp - 1) is overwritten w/ MSB of ret address if an ISR occurred
; 	sub 1
;     sbc a,a
;     and 1   ; (sp - 1) is not 0? return with pe, otherwise po
;     ret


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

_vdp_test:
	ld b, 0
_vdp_test_again:
	ld a, 080h
	call uart0_send
	ld a, 098h
	call uart0_send
	ld a, 00h
	call uart0_send
	djnz _vdp_test_again
	ret

eos_msx_machine_getscanline:
	push bc
	push hl
	ld bc,0
	ld c,a
	ld hl,_eos_msx_keyboard_scanline
	add hl,bc
	ld a, (hl)
	pop hl
	pop bc
	ret

eos_msx_machine_writeport:
	; interrupts enabled?
    ld a,i	; check IFF2, 1 is pe is EI, 0 is po is DI
    ; call System_CheckEIState
    push af     ; store interrupt state
    jp po, WRITE_PORT_WITH_INTERRUPTS_OFF
    di ;interrupts were on, we switch temporary off to write 2 or 3 consecutive bytes without interrupt
WRITE_PORT_WITH_INTERRUPTS_OFF:
    ; can we use short hand send?
    ld a, IYl ; port in A
    cp 0ffh
    jp nz, WRITE_PORT_REG_A
    ld a, c ; port in C
WRITE_PORT_REG_A:
    cp 098h
    jp z, WRITE_VDP_98h
    cp 099h
    jp z, WRITE_VDP_99h
    cp 0a8h
    jp z, WRITE_SLOT_REGISTER
    ; do normal 3 byte output
    ld a, 080h
    call uart0_send
    ld a, IYl ; port in A
    jp WRITE_PORT_UART
WRITE_VDP_98h:
    ld a, 10000010b ; output to vdp port 0
    jp WRITE_PORT_UART
WRITE_VDP_99h:
    ld a, 10010010b ; output to vdp port 1
    jp WRITE_PORT_UART    
WRITE_SLOT_REGISTER:
    ; TODO: save slot register
    jp WRITE_PORT_DONE
WRITE_PORT_UART:    
    ; A contains port
    call uart0_send
    ld a,iyh    ; value in IYh
    ; A contains value
    call uart0_send
WRITE_PORT_DONE:
    pop af ; restore interrupt state (in flags)
    ; interrupts were enabled? 1 is pe is EI, 0 is po is DI
    ret po ; leave them switched off
    ei     ; switch them back on when enabled
	ret