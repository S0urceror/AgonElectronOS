;
; Title:			AGON Electron OS - UART0 init, interrupt handling, putch, getch
; Author:			Mario Smit (S0urceror)
;

    INCLUDE	"equs.inc"
	INCLUDE "eZ80F92.inc"

    .ASSUME	ADL = 1
    
    DEFINE .STARTUP, SPACE = ROM
    SEGMENT .STARTUP

    ; C functions
    XDEF    _init_uart
    XDEF    _uart0_init_fifo
    XDEF    _getch
    XDEF    _putch
    XDEF	_uart0_handler

    ; ASM functions
    XDEF    uart0_recv_fifo_get
    XDEF    uart0_recv
    XDEF    uart0_send_fifo_add
    XDEF    uart0_send
    XDEF    uart0_recv_fifo_nrchars
    
    ; globals.asm
    XREF    _uart0_send_head
    XREF    _uart0_send_tail
    XREF    _uart0_send_buffer
    XREF    _uart0_recv_head
    XREF    _uart0_recv_tail
    XREF    _uart0_recv_buffer


PORTD_DRVAL_DEF       EQU    0ffh			;The default value for Port D data register (set for Mode 2).
PORTD_DDRVAL_DEF      EQU    0ffh			;The default value for Port D data direction register (set for Mode 2).
PORTD_ALT0VAL_DEF     EQU    0ffh			;The default value for Port D alternate register-0 (clear interrupts).
PORTD_ALT1VAL_DEF     EQU    000h			;The default value for Port D alternate register-1 (set for Mode 2).
PORTD_ALT2VAL_DEF     EQU    000h			;The default value for Port D alternate register-2 (set for Mode 2).


; baudrate divisors
; 18432000 / 16*1152000 = 1
BRD0_LOW                .equ    001h
BRD0_HIGH               .equ    000h
; baudrate divisors
; 18432000 / 16*115200 = 10
BRD1_LOW                .equ    00ah
BRD1_HIGH               .equ    000h

; interrupt driven sends or immediate sending?	
UART0_SEND_INTERRUPTS EQU    0

_init_uart
UART0_INIT:
    ; all pins to GPIO mode 2, high impedance input
    ld a, PORTD_DRVAL_DEF
    out0 (PD_DR),a
    ld a, PORTD_DDRVAL_DEF
    out0 (PD_DDR),a
    ld a, PORTD_ALT1VAL_DEF
    out0 (PD_ALT1),a
    ld a, PORTD_ALT2VAL_DEF
    out0 (PD_ALT2),a

    ; initialize for correct operation
    ; pin 0,1,2 and 3 to alternate function
    in0 a,(PD_DDR)
    or  00001111b   ; set pin 0,1,2,3 (RX,TX,CTS,RTS)
    out0 (PD_DDR), a
    in0 a,(PD_ALT1)
    and 11110000b   ; reset pin 0,1,2,3 (RX,TX,CTS,RTS)
    out0 (PD_ALT1), a
    in0 a,(PD_ALT2)
    or  00001111b   ; set pin 0,1,2,3 (RX,TX,CTS,RTS)
    out0 (PD_ALT2), a
    ; set baudrate (1152000) as a division of the clock signal
    in0 a,(UART0_LCTL)
    or 10000000b ; set UART_LCTL_DLAB
    out0 (UART0_LCTL),a
    ld a, BRD0_LOW ;// Load divisor low
    out0 (UART0_BRG_L),a
    ld a, BRD0_HIGH ;// Load divisor high
    out0 (UART0_BRG_H),a
    in0 a,(UART0_LCTL)
    and 01111111b ; reset UART_LCTL_DLAB
    out0 (UART0_LCTL),a
    ;
    ld a, 00000010b  ; multidrop, loopback, DTR disabled, RTS enabled
    out0 (UART0_MCTL),a
    ;
    ld a, 00000111b	 ; Turn on and clear HW FIFO, with trigger level of 1.
    out0 (UART0_FCTL),a
    ;
    in0 a, (UART0_LCTL)
    or  00000011b    ; 8 databits, 1 stopbit
    and 11110111b    ; no parity
    out0 (UART0_LCTL),a
    ;
    ld a, 00000001b  ; receive interrupt enabled, rest disabled
    out0 (UART0_IER),a

UART1_INIT:
    ; all pins to GPIO mode 2, high impedance input
    ld a, PORTD_DRVAL_DEF
    out0 (PC_DR),a
    ld a, PORTD_DDRVAL_DEF
    out0 (PC_DDR),a
    ld a, PORTD_ALT1VAL_DEF
    out0 (PC_ALT1),a
    ld a, PORTD_ALT2VAL_DEF
    out0 (PC_ALT2),a

    ; initialize for correct operation
    ; pin 0,1,2 and 3 to alternate function
    in0 a,(PC_DDR)
    or  00001111b   ; set pin 0,1,2,3
    out0 (PC_DDR), a
    in0 a,(PC_ALT1)
    and 11110000b   ; reset pin 0,1,2,3
    out0 (PC_ALT1), a
    in0 a,(PC_ALT2)
    or  00001111b   ; set pin 0,1,2,3
    out0 (PC_ALT2), a
    ; set baudrate (1152000) as a division of the clock signal
    in0 a,(UART1_LCTL)
    or 10000000b ; set UART_LCTL_DLAB
    out0 (UART1_LCTL),a
    ld a, BRD1_LOW ;// Load divisor low
    out0 (UART1_BRG_L),a
    ld a, BRD1_HIGH ;// Load divisor high
    out0 (UART1_BRG_H),a
    in0 a,(UART1_LCTL)
    and 01111111b ; reset UART_LCTL_DLAB
    out0 (UART1_LCTL),a
    ;
    ld a, 00000000b  ; multidrop, loopback, RTS, DTR disabled
    out0 (UART1_MCTL),a
    ;
    ld a, 00000111b	 ; HW fifo cleared and enabled, trigger level 1
    out0 (UART1_FCTL),a
    ;
    in0 a, (UART1_LCTL)
    or  00000011b    ; 8 databits, 1 stopbit
    and 11110111b    ; no parity
    out0 (UART1_LCTL),a
    ;
    ld a, 00000001b  ; receive interrupt enabled, rest disabled
    out0 (UART1_IER),a

    RET

;
; The C wrappers
;

; INT putch(INT ch);
;
; Write a character out to the UART
; Parameters:
; - ch: The character to write (least significant byte)
; Returns:
; - The character written
;
_putch:
    push ix
    ld ix,0
    add	ix,sp
    ld a, (ix+6) ; low byte of 3rd 3 byte element on the stack (ix, ra, arg0)
    call uart0_send_fifo_add
    ld hl,0
    ld l,a
    pop ix
    ret

; INT getch(VOID);
;
; Read a character out to the UART
; Returns:
; - The character read, 0 when no char available
;
_getch:
    call uart0_recv_fifo_get
    ld hl,0
    ret c
    ld l,a
    ret

_uart0_init_fifo:
    push hl
    push de
    ld de, _uart0_send_buffer
    ld hl, _uart0_send_head
    ld (hl),de
    ld hl, _uart0_send_tail
    ld (hl),de 
    ld de, _uart0_recv_buffer
    ld hl, _uart0_recv_head
    ld (hl),de
    ld hl, _uart0_recv_tail
    ld (hl),de     
    pop de
    pop hl
    ret

; Write a character to the SEND buffer
; Parameters:
; - A: The character to write (least significant byte)
; Returns:
; - A: The character written
uart0_send_fifo_add:
    push hl
    push de
    push af ; character to write
    ; store and increment head ptr
    ld hl, (_uart0_send_head)
    ld (hl),a
    inc hl
    ld (_uart0_send_head),hl
    ; wrap around?
    and a ; clear carry flag
    ld de, _uart0_send_buffer
    sbc hl,de
    ld a, l
    cp UART0_SEND_BUFFER_SIZE
    jr nz, _uart0_send_interrupt
    ; reset to start of buffer
    ld (_uart0_send_head),de
_uart0_send_interrupt:
    ; trigger send interrupt
    in0 a, (UART0_IER)
    or UART_IER_TRANSMITINT
    out0 (UART0_IER),a
    ; restore registers modified
    pop af ; character written
    pop de
    pop hl
    ret

; Write a character to the SEND buffer
; Parameters:
; - A: The character to write (least significant byte)
; Returns:
; - A: The character written
uart0_send:
    push af
_uart0_send_wait    
    ; check if host is ready to receive, otherwise wait
    in0 a, (UART0_MSR)
    bit 4,a ; check inverted CTS bit, 1 = CTS, 0 = NOT CTS (clear to send)
    jr z, _uart0_send_wait
    ;
    pop af
    out0 (UART0_THR),a
    RET
	
; Write a received character to the RECV buffer
; Parameters:
; - A: The character to write (least significant byte)
; Returns:
; - A: The character written
uart0_recv_fifo_add:
    push hl
    push de
    push af ; character to write
    ; store and increment head ptr
    ld hl, (_uart0_recv_head)
    ld (hl),a
    inc hl
    ld (_uart0_recv_head),hl
    ; wrap around?
    and a ; clear carry flag
    ld de, _uart0_recv_buffer
    sbc hl,de
    ld a, l
    cp UART0_RECV_BUFFER_SIZE
    jr nz, _uart0_recv_done
    ; reset to start of buffer
    ld (_uart0_recv_head),de
_uart0_recv_done:
    ; restore registers modified
    pop af ; character written
    pop de
    pop hl
    ret

; Get a character from the SEND fifo
; Returns:
; A - character in buffer
; F - carry when nothing in there
uart0_send_fifo_get:
    push hl
    push de
    ; first check if we have anything to return
    and a ; clear carry flag
    ld hl, (_uart0_send_head)
    ld de, (_uart0_send_tail)
    sbc hl,de
    jr z, _uart0_send_empty
    ; retrieve character and increment tail ptr
    ld a,(de)
    inc de
    ld (_uart0_send_tail),de
    ex hl,de
    push af ; store
    ; wrap around?
    and a ; clear carry flag
    ld de, _uart0_send_buffer
    sbc hl,de
    ld a, l
    cp UART0_SEND_BUFFER_SIZE
    jr nz, _uart0_send_read
    ; reset to start of buffer
    ld (_uart0_send_tail),de
_uart0_send_read:  
    pop af ; restore
    and a ; clear carry
    pop de
    pop hl
    ret    
_uart0_send_empty:    
    scf ; set carry
    pop de
    pop hl
    ret    

; Get a character from the RECV fifo
; Returns:
; A - character in buffer
; F - carry when nothing in there
uart0_recv_fifo_get:
    push hl
    push de
    ; first check if we have anything to return
    and a ; clear carry flag
    ld hl, (_uart0_recv_head)
    ld de, (_uart0_recv_tail)
    sbc hl,de
    jr z, _uart0_recv_empty
    ; retrieve character and increment tail ptr
    ld a,(de)
    inc de
    ld (_uart0_recv_tail),de
    ex hl,de
    push af ; store
    ; wrap around?
    and a ; clear carry flag
    ld de, _uart0_recv_buffer
    sbc hl,de
    ld a, l
    cp UART0_RECV_BUFFER_SIZE
    jr nz, _uart0_recv_read
    ; reset to start of buffer
    ld (_uart0_recv_tail),de
_uart0_recv_read:  
    pop af ; restore
    and a ; clear carry
    pop de
    pop hl
    ret    
_uart0_recv_empty:    
    scf ; set carry
    pop de
    pop hl
    ret        

; Get a character directly from the UART (blocking)
; Returns:
; A - character in buffer
uart0_recv:
_uart0_recv_loop
    ; while characters in fifo => process
    in0 a, (UART0_LSR)
    bit 0,a ; check receive data ready, 1 = character(s) in FIFO/RBR, 0 = empty
    jr	z, _uart0_recv_loop
    in0 a,(UART0_RBR)
    ret

uart0_recv_fifo_nrchars:
    push hl
    push de
    ; first check if we have anything to return
    and a ; clear carry flag
    ld hl, (_uart0_recv_head)
    ld de, (_uart0_recv_tail)
    sbc hl,de
    ld a, l
    pop de
    pop hl
    ret

; UART0 interrupt handler
;
_uart0_handler:		
    DI
    PUSH	AF
    PUSH    BC
    PUSH    DE
    PUSH    HL
    ; check UART interrupt flags
    in0		a, (UART0_IIR)
    and		00001110b ; mask interrupt level
    ;
_uart0_check_trigger_level:
    ; check trigger-level interrupt
    ; =============================
    cp		00000100b ; 0x04
    jr		nz, _uart0_handler_next ; no, skip data ready interrupt processing, TODO: handle error situations
    ;
    ; one or more characters have arrived, fifo level triggered
_uart0_hw_fifo_next: 
    ; while characters in fifo => process
    in0 a, (UART0_LSR)
    bit 0,a ; check receive data ready, 1 = character(s) in FIFO/RBR, 0 = empty
    jr		z,_uart0_handler_done
    in0 a,(UART0_RBR)
    ld BC,0
    ld c, a
    push BC
    CALL	uart0_recv_fifo_add
    POP BC
    jr		_uart0_hw_fifo_next
_uart0_handler_next:
    ; check transmit interrupt
    ; ========================
    cp      00000010b ; 0x02
    jr      nz, _uart0_handler_done ; no, skip data transmit processing
_uart0_handler_cts:
    ; check if host is ready to receive, otherwise wait
    in0 a, (UART0_MSR)
    bit 4,a ; check inverted CTS bit, 1 = CTS, 0 = NOT CTS (clear to send)
    jr z, _uart0_handler_cts
    ;
    call uart0_send_fifo_get
    jr c, _uart0_handler_transmit_done
    ; write byte
    out0 (UART0_THR),a
    jr _uart0_handler_cts
_uart0_handler_transmit_done:
    ; reset interrupt when empty
    in0 a,(UART0_IER)
    and 11111101b ; transmit interrupt
    out0 (UART0_IER),a    
_uart0_handler_done:
    POP     HL
    POP     DE
    POP     BC
    POP		AF
    EI
    RETI.L	
