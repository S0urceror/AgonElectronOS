    DEVICE NOSLOT64K
    PAGE 1

; EZ80 defines and helpers
UART0_MSR EQU 0c6h
UART0_LSR EQU 0c5h
UART0_THR EQU 0c0h
UART0_SPR EQU 0c7h
UART0_IER EQU 0c1h
UART0_RBR EQU 0C0h
PC_DR EQU 09Eh
PD_DR EQU 0A2h

    MACRO OUT0_A address
        ; out (address),a
        DB 0EDh
        DB 039h
        DB address
    ENDM
    MACRO IN0_A address
        ;in a,(address)
        DB 0EDh
        DB 038h
        DB address
    ENDM
; EZ80 defines and helpers

    ORG 8000H    
    jp c, WRITE_PORT_Z80
READ_PORT_Z80:
    POP IY      ; get address of following instruction
    INC IY
    PUSH IY     ; return address is next byte
    PUSH BC     ; preserve BC
    ; interrupts enabled?
    ld a,i	; check IFF2, 1 is pe is EI, 0 is po is DI
    push af     ; store interrupt state
    jp po, READ_PORT_WITH_INTERRUPTS_OFF
    di ;interrupts were on, we switch temporary off to write 2 or 3 consecutive bytes without interrupt
READ_PORT_WITH_INTERRUPTS_OFF:
    ; can we use short hand send?
    ld a, (IY-1) ; port in A
    cp 0ffh
    jp nz, READ_PORT_REG_A
    ld a, c ; port in C
READ_PORT_REG_A:
    ld b, a ; store port in B
    ; switch off uart0 receive interrupts
	; we go direct mode
	IN0_A UART0_IER
	and 0feh ;0b11111110
    OUT0_A UART0_IER
    ;
    call UART0_WAIT_AVAILABLE
    ld a, 081h // read single
    OUT0_A UART0_THR
    ld a, b ; get port from B
    ; A contains port
    OUT0_A UART0_THR
    ; get value
READ_PORT_recv_loop
    ; while characters in fifo => process
    IN0_A UART0_LSR
    bit 0,a ; check receive data ready, 1 = character(s) in FIFO/RBR, 0 = empty
    jr	z, READ_PORT_recv_loop
    IN0_A UART0_RBR
    ld b,a ; store value in B
    ; switch on uart0 receive interrupts
	IN0_A UART0_IER
	or 001h ;0b00000001
    OUT0_A UART0_IER
READ_PORT_DONE:    
    pop af ; restore interrupt state (in flags)
    ld a, b ; restore value from B
    pop bc ; restore BC
    ; interrupts were enabled? 1 is pe is EI, 0 is po is DI
    ret po ; leave them switched off
    ei     ; switch them back on when enabled
    ret
WRITE_PORT_Z80:
    POP IY      ; get address of following instruction
    INC IY
    PUSH IY     ; return address is next byte
    PUSH BC     ; preserve BC
    ld b, a     ; value to write in B
    ; interrupts enabled?
    ld a,i	; check IFF2, 1 is pe is EI, 0 is po is DI
    push af     ; store interrupt state
    jp po, WRITE_PORT_WITH_INTERRUPTS_OFF
    di ;interrupts were on, we switch temporary off to write 2 or 3 consecutive bytes without interrupt
WRITE_PORT_WITH_INTERRUPTS_OFF:
    ; can we use short hand send?
    ld a, (IY-1) ; port in A
    cp 0ffh
    jp nz, WRITE_PORT_REG_A
    ld a, c ; port in C
WRITE_PORT_REG_A:
    push af
    ; uart0 send
    call UART0_WAIT_AVAILABLE    
    ld a, 080h
    OUT0_A UART0_THR
    pop af
WRITE_PORT_UART:    
    ; A contains port
    OUT0_A UART0_THR
    ld a,b ; get value to write from B
    ; A contains value
    OUT0_A UART0_THR
WRITE_PORT_DONE:
    pop af ; restore interrupt state (in flags)
    ld a,b ; restore A
    pop bc ; restore BC
    ; interrupts were enabled? 1 is pe is EI, 0 is po is DI
    ret po ; leave them switched off
    ei     ; switch them back on when enabled
    ret
UART0_WAIT_AVAILABLE:
_uart0_send_wait:
    ; check if host is ready to receive, otherwise wait
    IN0_A UART0_MSR
    bit 4,a ; check inverted CTS bit, 1 = CTS, 0 = NOT CTS (clear to send)
    jr z, _uart0_send_wait
_uart0_send_wait2:
    ; check if send buffer is ready
    IN0_A UART0_LSR
    bit 5,a ; 0x20 - THREMPTY - transmit holding register empty, fifo still sending or empty
            ; 0x40 - TEMT - transmit holding register and fifo empty
    jr z, _uart0_send_wait2
    ret    