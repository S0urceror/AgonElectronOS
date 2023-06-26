;
; Title:	AGON MOS - Interrupt handlers
; Author:	Dean Belfield
; Created:	03/08/2022
; Last Updated:	29/03/2023
;
; Modinfo:
; 09/03/2023:	No longer uses timer interrupt 0 for SD card timing
; 29/03/2023:	Added support for UART1

			INCLUDE	"macros.inc"
			INCLUDE	"equs.inc"
			INCLUDE "eZ80F92.inc"

			.ASSUME	ADL = 1

			DEFINE .STARTUP, SPACE = ROM
			SEGMENT .STARTUP
			
			XDEF	_vblank_handler
			XREF	_clock
			
; Vertical Blank Interrupt handler
;
_vblank_handler:	
			DI
			PUSH	AF
			PUSH	BC
			PUSH	DE
			PUSH	HL	
			SET_GPIO 	PB_DR, 2		; Need to set this to 2 for the interrupt to work correctly
			LD 		HL, (_clock)		; Increment the 32-bit clock counter
			LD		BC, 2			; By 2, effectively timing in centiseconds
			ADD		HL, BC
			LD		(_clock), HL
			LD		A, (_clock + 3)
			ADC		A, 0
			LD		(_clock + 3), A			
			POP		HL
			POP		DE
			POP		BC
			POP		AF
			EI	
			RETI.L
			
