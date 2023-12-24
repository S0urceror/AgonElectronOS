/*
 * Title:			AGON Electron OS - Timer
 * Author:			Dean Belfield
 */

#include <eZ80.h>
#include <defines.h>

#include "timer.h"

// Configure Timer 0
// Parameters:
// - interval: Interval in ms
// - clkdiv: 4, 16, 64 or 256
// - clkflag: Other clock flags (interrupt, etc)
// Returns:
// - interval value
//
unsigned short init_timer0(int interval, int clkdiv, unsigned char ctrlbits) {
	unsigned short	rr;
	unsigned char	clkbits = 0;
	unsigned char	ctl;

	switch(clkdiv) {
		case  16: clkbits = 0x04; break;
		case  64: clkbits = 0x08; break;	
		case 256: clkbits = 0x0C; break;
	}
	
	ctl = (ctrlbits | clkbits);
	rr = (unsigned short)((SysClkFreq / 1000) / clkdiv) * interval;

	TMR0_CTL = 0x00;													// Disable the timer and clear all settings	
	TMR0_RR_L = (unsigned char)(rr);
	TMR0_RR_H = (unsigned char)(rr >> 8);
    TMR0_CTL = ctl;

	return rr;
}

// Enable Timer 0
// Parameters:
// - enable: 0 = disable, 1 = enable
//
void enable_timer0(unsigned char enable) {
	unsigned char b;

	if(enable <= 1) {
		b = TMR0_CTL;
		b &= 0xFC;
		b |= (enable | 2); 
		TMR0_CTL = b;	
	}
}

// Get data count of Timer 0
//
unsigned short get_timer0() {
	unsigned char l = TMR0_DR_L;
	unsigned char h = TMR0_DR_H;
	return (h << 8) | l;
}

// Configure Timer 1
// Parameters:
// - interval: Interval in ms
// - clkdiv: 4, 16, 64 or 256
// - clkflag: Other clock flags (interrupt, etc)
// Returns:
// - interval value
//
unsigned short init_timer1(int interval, int clkdiv, unsigned char ctrlbits) {
	unsigned short	rr;
	unsigned char	clkbits = 0;
	unsigned char	ctl;

	switch(clkdiv) {
		case  16: clkbits = 0x04; break;
		case  64: clkbits = 0x08; break;	
		case 256: clkbits = 0x0C; break;
	}
	ctl = (ctrlbits | clkbits);

	rr = (unsigned short)((SysClkFreq / 1000) / clkdiv) * interval;

	TMR1_CTL = 0x00;													// Disable the timer and clear all settings	
	TMR1_RR_L = (unsigned char)(rr);
	TMR1_RR_H = (unsigned char)(rr >> 8);
    TMR1_CTL = ctl;

	return rr;
}

// Enable Timer 1
// Parameters:
// - enable: 0 = disable, 1 = enable
//
void enable_timer1(unsigned char enable) {
	unsigned char b;

	if(enable <= 1) {
		b = TMR1_CTL;
		b &= 0xFC;
		b |= (enable | 2); 
		TMR1_CTL = b;	
	}
}

// Get data count of Timer 1
//
unsigned short get_timer1() {
	unsigned char l = TMR1_DR_L;
	unsigned char h = TMR1_DR_H;
	return (h << 8) | l;
}