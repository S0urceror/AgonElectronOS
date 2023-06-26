;
; Title:	AGON Electron OS - Globals
; Author:	Dean Belfield
; Modified: Mario Smit (S0urceror)

			INCLUDE	"../src/equs.inc"
			
			XDEF	_clock
			XDEF	_rtc 
			XDEF 	_coldBoot
			XDEF	_gp
			XDEF	_callSM
			XDEF    SLOT_REGISTER
			XDEF	call_address_ix
			XDEF	_prev_mbase
			XDEF	_uart0_send_buffer
			XDEF	_uart0_send_head 
			XDEF	_uart0_send_tail
			XDEF    _uart0_recv_head
			XDEF    _uart0_recv_tail
			XDEF    _uart0_recv_buffer			

			SEGMENT BSS		; This section is reset to 0 in cstartup.asm
			
_clock			DS	4		; + 00h: Clock timer in centiseconds (incremented by 2 every VBLANK)
_coldBoot:		DS	1		; extern char _coldBoot
_gp:			DS	1		; extern char _gp
_rtc:			DS	8		; + 1Ah: Real time clock data
_callSM:		DS	5		; Self-modding code for CALL.IS (HL)
SLOT_REGISTER	DS  1		
call_address_ix DS  3
_prev_mbase		DS  1

_uart0_send_buffer DS UART0_SEND_BUFFER_SIZE
_uart0_send_head DS 3
_uart0_send_tail DS 3
_uart0_recv_buffer DS UART0_RECV_BUFFER_SIZE
_uart0_recv_head DS 3
_uart0_recv_tail DS 3
		
			SECTION DATA		; This section is copied to RAM in cstartup.asm


			END