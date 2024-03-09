;----------------------------------------------------------
;		msxromcrt0.s - by Danilo Angelo, 2020-2023
;
;		Template for ROM (cartridges) programs for MSX 
;		Derived from the work of mvac7/303bcn
;----------------------------------------------------------

	.include "MSX/BIOS/msxbios.s"
	.include "targetconfig.s"
	.include "applicationsettings.s"

	.globl	_main

.if GLOBALS_INITIALIZER
	.globl  l__INITIALIZER
    .globl  s__INITIALIZED
    .globl  s__INITIALIZER
.endif

.if DEVICE_EXPANSION
    .globl  _device_expansion
.endif

STR_COMPARE = 0

;   ====================================
;   ========== HEADER SEGMENT ==========
;   ====================================
	.area	_HEADER (ABS)
  	.org	#fileStart

;----------------------------------------------------------
;	ROM Header
;----------------------------------------------------------
	.db		#0x41				; ROM ID
	.db		#0x42				; ROM ID
	.dw		#_init				; Program start
.if CALL_EXPANSION
	.dw		#_call_expansion	; BASIC's CALL instruction expansion routine
.else
	.dw		#0x0000				; BASIC's CALL instruction not expanded
.endif
.if DEVICE_EXPANSION
	.dw		#_device_expansion	; BASIC's IO DEVICE expansion routine
.else
	.dw		#0x0000				; BASIC's IO DEVICE not expanded
.endif
	.dw		#BASIC_PROGRAM		; BASIC program
	.dw		#0x0000				; Reserved
	.dw		#0x0000				; Reserved
	.dw		#0x0000				; Reserved

;----------------------------------------------------------
;	Program start
;----------------------------------------------------------
_init::

.if LATE_EXECUTION
;----------------------------------------------------------
;	Configure H.STKE hook if LATE_EXECUTION is _ON.
	ld		a, c				; Get the ROM slot number
	ld		hl,	#H_CLEA_romInit
	ld		de,	#BIOS_H_CLEAR
	ld		bc,	#H_CLEA_romInit_end - H_CLEA_romInit
	ldir						; Copy the routine to execute the ROM to the hook
	ld		(#BIOS_H_CLEAR+1), a	; Put the ROM slot number to the hook
	ret							; Back to slots scanning

H_CLEA_romInit::				; Routine to execute the ROM
	rst		#BIOS_CALLF			; Inter-slot call
	.db		#0					; Dummy; to be replaced with the slot number of ROM in RAM
	.dw		#_romInit			; Address to execute the ROM
H_CLEA_romInit_end:
.endif

_romInit::
.if LATE_EXECUTION
.if RETURN_TO_BASIC
 	; save register to support returning to BASIC and LATE_EXECUTION
    push ix
    push iy
    push hl             ; save BASIC pointer
    push de
    push bc
    push af
.endif
;----------------------------------------------------------
;	Step 0: Remove hook H.STKE
	ld		hl, #BIOS_H_CLEAR
	ld		de, #BIOS_H_CLEAR + 1
	ld		bc,	#H_CLEA_romInit_end - H_CLEA_romInit - 1
	ld		a, #0xC9			; ret
	ld		(hl), a				; Remove the hook 
	ldir
.endif

;----------------------------------------------------------
;	Step 1: Initialize heap pointer
	ld		hl, #_HEAP_start
	ld		(#_heap_top), hl

;----------------------------------------------------------
;	Step 2: Enables page 2 on the same slot/subslot as page 1
;           (useful for 32kb ROMs on pages 1 and 2)
.if SET_PAGE_2
	di
	call	#BIOS_RSLREG
	rrca
	rrca
	and		#0x03	
	ld		c, a
	ld		hl, #BIOS_EXPTBL
	add		a, l
	ld		l, a
	ld		a, (hl)
	and		#0x80
	or		c
	ld		c, a
	inc		l
	inc		l
	inc		l
	inc		l
	ld		a, (hl)
	and		#0x0c
	or		c
	ld		h, #0x80
	call	#BIOS_ENASLT
	ei
.endif

;----------------------------------------------------------
;	Step 3: Sets stack to HIMEM
.ifeq RETURN_TO_BASIC
.if STACK_HIMEM
	di
	ld		sp, (#BIOS_HIMEM)		;Stack at the top of memory.
	ei
.endif
.endif

;----------------------------------------------------------
;	Step 4: Initialize globals
.if GLOBALS_INITIALIZER
	call    gsinit
.endif

;----------------------------------------------------------
;	Step 5: Run application
.if RETURN_TO_BASIC
	call	_main
.if LATE_EXECUTION	
    pop af
    pop bc
    pop de
    pop hl ; restore BASIC pointer
    pop iy
    pop ix
.endif
	ret
.else
	call	_main
	jp		BIOS_CHKRAM
.endif 

;----------------------------------------------------------
;	Segments order
;----------------------------------------------------------
	.area _CODE
	.area _HOME
	.area _GSINIT
	.area _GSFINAL
	.area _INITIALIZER
	.area _ROMDATA
	.area _DATA
	.area _INITIALIZED
	.area _HEAP

;   ==================================
;   ========== HOME SEGMENT ==========
;   ==================================
	.area _HOME

;----------------------------------------------------------
;	Support for MSXBasic CALL command expansion
.if CALL_EXPANSION
STR_COMPARE = 1
_call_expansion::
	exx
	ld		hl, #callStatementIndex
	jr		callExpansionParseStmt

callExpansionStmtNotFound:
	pop hl

callExpansionParseStmt:	
;	get pointer to statement in table
	xor		a
	ld		e, (hl)
	inc		hl
	ld		d, (hl)
	cp		e
	jr nz,	callExpansionNotEndOfList
	cp		d
	jr nz,	callExpansionNotEndOfList
;	statement not found; end expansion
	exx
	scf
	ret

callExpansionNotEndOfList:
	inc		hl
	push	hl

;	get pointer to statement in CALL
	ld		hl, #BIOS_PROCNM
	call	compareString
	jr z,	callExpansionStmtFound		
	cp		#0
	jr nz,	callExpansionStmtNotFound
	ld		a, b
	cp		#0x20
	jr nz,	callExpansionStmtNotFound

callExpansionStmtFound:
;	statement found; execute and exit
	pop		hl
	inc		de
	push	de
	exx
	pop		de				; *handler
	ld		(#_pBuffer), hl

.ifeq __SDCCCALL
	ld		hl, #_pBuffer
	push	hl				; parameters
.endif

	ld		hl, #callExpansionFinalize
	push	hl				; finalize
	ex		de, hl
	ld		e, (hl)
	inc		hl
	ld		d, (hl)

.if __SDCCCALL
	ld		hl, #_pBuffer
.endif

	push	de				; handler
	ret						; calls handler with return to finalize below
							; handler must return hl pointing to end of command (end of line or ":")
	
callExpansionFinalize:
; at this point, pBuffer must be pointing to end of command (end of line or ":")
.ifeq __SDCCCALL
	ld		a, l
	pop		hl
.endif
;	pop		hl
	or		a
	jr z,	callExpansionFinalizeNoError
callExpansionFinalizeError:
	scf
	ret
callExpansionFinalizeNoError:
	ld		hl, (#_pBuffer)
	ret
.endif

;----------------------------------------------------------
;	Support for MSX-Basic DEVICES expansion
;
;
.if DEVICE_EXPANSION
; deviceIndex contains the list of configured devices
; 00  01  02  03  04  05
; DL1 DH1 DH2 DL2 DH3 DL3
;
; DHx_DLx = 0000h => no device configured or end of list
; DHx_DLx = 1234h => points to device control block at 1234h
;
; device control block contains
; + null terminated string with devicename
; + LLHH - INIT
; + LLHH - OPEN
; + LLHH - CLOSE
; + LLHH - RANDOM_ACCESS
; + LLHH - OUTPUT
; + LLHH - INPUT
; + LLHH - LOC
; + LLHH - LOF
; + LLHH - EOF
; + LLHH - FPOS
; + LLHH - BACKUP_CHAR
STR_COMPARE = 1
_device_expansion::
	cp  	#0xff ; FF = find corresponding device, otherwise file operation requested
	jr nz,  deviceExpansionHandler
	push    hl
	push    de
	push    bc
	ld      b, #0
	ld      hl, #deviceIndex
	jr      _check_device_name
_next_device:
	inc     b
_check_device_name:
	ld      e, (hl)
	inc     hl
	ld      d, (hl)
	inc     hl ; points to next device entry
	ld      a, e
	or      d
	jr      z, _device_not_found
	;
	; we have found a valid device control block pointed to by DE
	push    hl ; store device pointer
	push    bc ; store counter
	ld      hl, #22
	add     hl, de   
	ex      de, hl ; DE to position of device string in device control block
	ld      hl, #BIOS_PROCNM
	call    compareString
	pop     bc ; restore counter
	pop     hl ; restore device pointer
	; did we find it?
	jr      nz, _next_device ; NO, try again
	jr      z, _device_found ; YES, we did it
_device_not_found:
	; return not found	
	scf 
	pop     bc 
	pop     de
	pop     hl
	ret

_device_found:
	inc     de ; DE points now to INIT function in device control block
	ld      a, (de)
	ld      l,a
	inc     de
	ld      a, (de)
	ld      h,a
	push bc ; protect device counter
	ld de,  #_device_found_done
	push de ; where we continue
	push hl ; jump to INIT routine
	ret
_device_found_done:
	pop bc ; restore device counter
	ld a, b ; return in A
	and a ; clear carry
	; restore old BC, DE and HL
	pop     bc 
	pop     de
	pop     hl
	ret

deviceExpansionHandler:
	exx 	; swap registers
	ex      af,af' ; swap accumulator
	
	ld		a, (#BIOS_DEVICE)
	sla     a ; times two
	ld      l,a
	ld      h,#0
	ld      de, #deviceIndex
	add     hl, de
	; HL now points to the right device entry
	ld      e, (hl)
	inc     hl
	ld      d, (hl)
	; DE now points to the right device control block
	; lets find the right operation function
	ex      af,af' ; swap accumulator, back to operation number
	ld      l,a
	ld      h,#0
	add     hl, de
	ld      e, (hl)
	inc     hl
	ld      d, (hl)
	push    de ; push operation function address to the stack
	exx		; swap all registers back to what they were before calling us
	ret     ; continue at operation function

.endif

.if STR_COMPARE
compareString::
	ld		a, (hl)
	ld		b, a
	ld		a, (de)
	cp		b
	ret nz
	cp		#0
	ret z
	inc		hl
	inc		de
	jr		compareString
.endif

;   =====================================
;   ========== GSINIT SEGMENTS ==========
;   =====================================
.if GLOBALS_INITIALIZER
	; Note: not possible to #if l__INITIALIZER eq 0 because
	; it depends on the assembly/compilation of other modules

	.area	_GSINIT
gsinit::
    ld      bc,#l__INITIALIZER
    ld      a,b
    or      a,c
    jp	z,  gsinit_next
    ld	    de,#s__INITIALIZED
    ld      hl,#s__INITIALIZER
    ldir

	.area	_GSFINAL
gsinit_next:
    ret
.endif
	
;   ======================================
;   ========== ROM_DATA SEGMENT ==========
;   ======================================
	.area	_ROMDATA
.if CALL_EXPANSION
	MCR_CALLEXPANSIONINDEX
.endif

.if DEVICE_EXPANSION
	MCR_DEVICEEXPANSIONINDEX
.endif

;   ==================================
;   ========== DATA SEGMENT ==========
;   ==================================
	.area	_DATA
_heap_top::
	.blkw	1

.if CALL_EXPANSION
_pBuffer::
	.blkw	1
.endif

;   ==================================
;   ========== HEAP SEGMENT ==========
;   ==================================
	.area	_HEAP
_HEAP_start::
