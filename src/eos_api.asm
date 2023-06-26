	INCLUDE "ez80f92.inc"
	.ASSUME ADL=1

	;DEFINE .STARTUP, SPACE = ROM
    SEGMENT CODE ;.STARTUP

	XDEF	electron_os_api

	XREF	_machine_read_write_disk
    XREF 	_machine_warm_boot
	;
	XREF	SLOT_REGISTER
	XREF	call_address_ix
	XREF    _prev_mbase
	XREF	_callSM

JUMPER EQU 0f3fch ;Work area of the data recorder. (Until MSX2+) 

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
    ;
    DW eos_msx_machine_slotregister				;0x0004
    DW eos_msx_machine_rdslt					;0x0006
    DW eos_msx_machine_wrslt					;0x0008
    DW eos_msx_machine_calslt					;0x000a
    DW eos_msx_machine_enaslt					;0x000c

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
	; ROM, slot 0, in 0x50000-0x5ffff , other slots RAM 0x60000 - 0x8ffff
	and a
	jr z, _rdslt_rom
	push hl
	; add start ram slot
	add a, 5
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
	ret
_rdslt_rom
	ld a, 0aah
	ret
eos_msx_machine_wrslt:
	and a
	ret z ; do not attempt to write ROM section
	push hl
	; add start ram slot
	add a, 5
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
	ret
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
	add a,5
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

