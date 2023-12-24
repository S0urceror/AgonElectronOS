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
#include "joysticks.h"

UINT8 errno;
static FATFS 	fs;						// Handle for the file system

// Initialise the interrupts
//
void init_interrupts(void) 
{
	set_vector(PORTB1_IVECT, generic_vblank_handler); 	// 0x32
	set_vector(UART0_IVECT,  uart0_handler);			// 0x18
}

#define VDU_SYSTEM          0
#define VDU_GP              0x80
#define VDU_MODE            0x86
#define OS_MOS              1
#define OS_ELECTRON         2
#define CTRL_W 				0x17 // 23 decimal, MOS escape code, ElectronOS 8 bits ASCII value

BOOL waitESP32 ()
{
	int i,t;
	UINT8 ch=0;
	BOOL esp32_found = FALSE;
	
	init_timer0(10, 16, 0x00);  		// 10ms timer for delay
	for(t = 0; t < 200; t++) 
	{			
		// A timeout loop (200 x 50ms = 10s)
		// Send a general poll packet
		putch(CTRL_W);						
		putch(VDU_SYSTEM);
		putch(VDU_GP);
		putch(OS_ELECTRON);
		// Wait 50ms
		for(i = 0; i < 5; i++) 
		{
			wait_timer0();
			ch = getch();
			if (ch!=0)
				break;
		}
		// Check response
		if (ch==VDU_GP)
		{
			if (getch()==1)
			{
				// echoed back to us
				if (getch()==OS_ELECTRON)
				{
					esp32_found = TRUE;
					break;				// If general poll returned, then exit for loop
				}
			}
		}
	}
	enable_timer0(0);					// Disable the timer

	return esp32_found;
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
	init_joysticks ();
	EI();											// Enable the interrupts now
	
	// wakeup sequence
	if (!waitESP32())
	{
		printf ("[ESP32 not answering]\r\n");
		printf ("[System halted]\r\n");
		while (1);
	}

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