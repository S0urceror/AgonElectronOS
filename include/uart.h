#ifndef __UART_H_
#define __UART_H_

// in uart_.asm
extern void 	init_uart();
extern void 	uart0_handler(void);
extern INT 		getch ();
extern INT 		putch (INT);
extern INT 		putch_direct (INT);
void            uart0_init_fifo ();

#endif