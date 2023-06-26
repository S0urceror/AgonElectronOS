/*
 * Title:			AGON Electron OS
 * Author:			Mario Smit (S0urceror)
 */

#include <eZ80.h>
#include <defines.h>
#include <stdio.h>

#include "globals.h"
#include "spi.h"
#include "uart.h"
#include "machine.h"
#include "interrupts.h"
#include "vectors16.h"
#include "rtc.h"
#include "timer.h"
#include "cp.h"

UINT8 errno;
static FATFS 	fs;						// Handle for the file system

// Initialise the interrupts
//
void init_interrupts(void) 
{
	set_vector(PORTB1_IVECT, vblank_handler); 	// 0x32
	set_vector(UART0_IVECT,  uart0_handler);	// 0x18
}


// The main loop
//
int main(void) {
	INT		ch;

	DI();											// Ensure interrupts are disabled before we do anything
	init_interrupts();								// Initialise the interrupt vectors
	init_rtc();										// Initialise the real time clock
	init_spi();										// Initialise SPI comms for the SD card interface
	uart0_init_fifo ();
	init_uart ();									// Initialize UARTs
	EI();											// Enable the interrupts now
	
	// wait 0.5 seconds or 500ms
	//init_timer1(500, 16, 0x00);  // 10ms timer for delay
	//wait_timer1();
	//enable_timer1(0);

	// wakeup sequence
	while(getch ()!=ESC);
	putch (CTRL_Z);

	// let's go...
	printf("Electron - OS - version %d.%d.%d\r\n", OS_major, OS_minor,OS_revision);

	if (f_mount(&fs, "", 1)==FR_OK)							// Mount the SD card
	{
		printf ("[SD card mounted]\r\n");
		machine_init ();
	}
	else
	{
		printf ("[SD card NOT mounted]\r\n");
		printf ("[System halted]\r\n");
		while (1);
	}

	// command processor
	cp_run ();

	return 0;
}