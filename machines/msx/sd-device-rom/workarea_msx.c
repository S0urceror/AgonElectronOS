#include "MSX/BIOS/msxbios.h"
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>

#include "workarea.h"
#include "workarea_msx.h"

#pragma disable_warning 85
const uint8_t byte_in_rom_space=0;

uint8_t get_slot_id (uint16_t address)  __z88dk_fastcall __naked
{
    __asm
; h = memory address high byte (bits 6-7: page)
; a <- slot ID formatted FxxxSSPP
; Modifies: f, bc, de
    call BIOS_RSLREG
    bit 7,h
    jr z,PrimaryShiftContinue
    rrca
    rrca
    rrca
    rrca
PrimaryShiftContinue:
    bit 6,h
    jr z,PrimaryShiftDone
    rrca
    rrca
PrimaryShiftDone:
    and #0b00000011
    ld c,a
    ld b,#0
    ex de,hl
    ld hl,#BIOS_EXPTBL
    add hl,bc
    ld a,(hl)
    and #0x80
    or c
    ld c,a
    inc hl  ; move to SLTTBL
    inc hl
    inc hl
    inc hl
    ld a,(hl)
    ex de,hl
    bit 7,h
    jr z,SecondaryShiftContinue
    rrca
    rrca
    rrca
    rrca
SecondaryShiftContinue:
    bit 6,h
    jr nz,SecondaryShiftDone
    rlca
    rlca
SecondaryShiftDone:
    and #0b00001100
    or c
    ld l, a
    ret

    __endasm;
}

uint8_t* getsltwrk ()
{
    // slot ID formatted FxxxSSPP
    uint8_t slot_id = get_slot_id ((uint16_t) &byte_in_rom_space);
    uint8_t page = ((uint16_t) &byte_in_rom_space)>>14;
    //bool extended = slot_id&0b10000000;
    uint8_t primary = slot_id&0b00000011;
    uint8_t secondary = (slot_id>>2) & 0b00000011;
    uint8_t vector = primary*16*2+secondary*4*2+page;
    uint16_t sltwrkaddress = 0xfd09+vector;
    return (uint8_t*) sltwrkaddress;
}

WORKAREA* get_workarea() __z88dk_fastcall __naked
{
    __asm
    call _getsltwrk
    push hl
    pop iy
    ld l, 0(iy)
    ld h, 1(iy)
    ret
    __endasm;
}
void set_workarea (WORKAREA* wrk) __z88dk_fastcall __naked
{
    __asm
    push hl
    call _getsltwrk
    push hl
    pop iy
    pop hl
    ld 0(iy),l
    ld 1(iy),h
    ret
    __endasm;
}

void init_workarea ()
{
    WORKAREA* wrk = get_workarea ();
    wrk->basic_pointer = 0x0000;
}

void allocate_workarea (uint16_t size) __z88dk_fastcall __naked
{
    __asm
    call ALLOC
    jr c, HALT
    call _set_workarea
    ret

HALT:
    di
    halt

ALLOC:
	ld	a,l		;is requested size 0?
	or	h
	ret	z		;yes, allocation always succeeds
	ex	de,hl	;calculate -size
	ld	hl,#0
	sbc	hl,de
	ld	c,l		;remember specified size
	ld	b,h
	add	hl,sp   ;[HL] = [SP] - size
	ccf
	ret	c		;size too big

	ld	a,h
	cp	#0xC2   ;high(BOOTAD)
	ret	c		;no room left

	ld	de,(#BIOS_BOTTOM)	;get current RAM bottom
	sbc	hl,de		;get memory space left after allocation
	ret	c		;no space left
	ld	a,h		;do we still have breathing room?
	cp	#2       ;high(512)
	ret	c		;no, not enough space left
;
;       Now, requested size is legal, begin allocation
;
	push bc		;save -size
	ld	hl,#0
	add	hl,sp		;get current stack pointer to [HL]
	ld	e,l		;move source address to [DE]
	ld	d,h
	add	hl,bc
	push	hl		;save destination
	ld	hl,(#BIOS_STKTOP)
	or	a
	sbc	hl,de
	ld	c,l		;move byte count to move to [BC]
	ld	b,h
	inc	bc
	pop	hl		;restore destination
	ld	sp,hl		;destination becomes the new SP
	ex	de,hl
	ldir			;move stack contents
	pop	bc		;restore -size
	ld	hl,(#BIOS_HIMEM)
	add	hl,bc
	ld	(#BIOS_HIMEM),hl
	ld	de,#-2*(2+9+256)
	add	hl,de
	ld	(#BIOS_FILTAB),hl	;pointer to first FCB
	ex	de,hl
	ld	hl,(#BIOS_MEMSIZ)	;update MEMSIZ
	add	hl,bc
	ld	(#BIOS_MEMSIZ),hl
	ld	hl,(#BIOS_NULBUF)	;update NULBUF
	add	hl,bc
	ld	(#BIOS_NULBUF),hl
	ld	hl,(#BIOS_STKTOP)	;update STKTOP
	add	hl,bc
;
;       Re-build BASICs file structures
;
	ld	(#BIOS_STKTOP),hl
	dec	hl		;and SAVSTK
	dec	hl
	ld	(#BIOS_SAVSTK),hl
	ld	l,e		;get FILTAB in [HL]
	ld	h,d
	inc	hl		;point to first FCB
	inc	hl
	inc	hl
	inc	hl
	ld	a,#2
DSKFLL:
	ex	de,hl
	ld	(hl),e		;set address in FILTAB
	inc	hl
	ld	(hl),d
	inc	hl
	ex	de,hl
	ld	bc,#7
	ld	(hl),b		;make it look closed
	add	hl,bc
	ld	(hl),b		;clear flag byte
	ld	bc,#9+256-7
	add	hl,bc		;point to next FCB
	dec	a
	jr	nz,DSKFLL
	ret
    __endasm;
}