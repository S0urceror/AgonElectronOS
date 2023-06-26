#ifndef __UART_H_
#define __UART_H_

// in uart_.asm
extern void 	init_uart();
extern void 	uart0_handler(void);
extern INT 		getch ();
extern INT 		putch (INT);
void            uart0_init_fifo ();

// receive
//void uart0_recv_fifo_init ();
//INT  uart0_recv_fifo_add (INT ch);
//INT  uart0_recv_fifo_get ();
// send
//void uart0_send_fifo_init ();

//INT  uart0_send_fifo_add (INT ch);
//INT  uart0_send_fifo_get ();
//INT  uart0_send_fifo_bytes (char* buffer,UINT8 len);
#endif