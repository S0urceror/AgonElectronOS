// ----------------------------------------------------------
//		devicehandler.c - by Danilo Angelo, 2023
//
//		BASIC's DEVICE handler example
//		C version
//
//      This file is only needed when implementing
//      a new BASIC DEVICE and may be ignored otherwise.
// ----------------------------------------------------------

#include "applicationsettings.h"

#ifdef DEVICE_EXPANSION

#include "printinterface.h"
#include "MSX/BIOS/msxbios.h"
#include <stdint.h>
#include <stdio.h>

__at (BIOS_DEVICE) char req_device;
__at (BIOS_FILNAM) char filename[];
__at (BIOS_FILNM2) char filename2;
__at (BIOS_PTRFIL) uint16_t ptrfil;
__at (BIOS_DAC) uint8_t DAC[];
__at (BIOS_VALTYP) uint16_t VALTYP;

char* ptrHello;
char szHello[] = "Hello world!!!";

void onDeviceAGON_INIT() 
{
	print ("AGON INIT\r\n\0");
}
void onDeviceAGON_OPEN (uint16_t reghl,uint16_t regde)
{
	char szBuffer[100];

	char* p = (char*) reghl;
	uint8_t regE = (uint8_t)(regde&0xff);
	ptrHello = szHello;
	
	sprintf (szBuffer,"AGON OPEN - DE:%x,HL:%x\r\n\0",regde,reghl);
	print (szBuffer);
	p[0] = regE; 			// store openmode at index 0
	p[1] = 1; 				// store channelnumber at index 1
	ptrfil = reghl;
}
void onDeviceAGON_CLOSE (uint16_t reghl)
{
	char* p = (char*) reghl;
	print ("AGON CLOSE\r\n\0");
	p[0] = 0;
}

// Input:
//  HL: filebuffer
//  DE: BASIC pointer
//  B: 00h = GET, 80h = PUT
// Output:
//  HL: BASIC pointer (updated)
void onDeviceAGON_RANDOM_ACCESS (uint16_t reghl,uint16_t regde)
{
	__asm
		push de			; store BASIC pointer from DE
		ld a, b
		and a
		jr z, get_record
	put_record:
		ld hl, #strput
		jr print
	get_record:
		ld hl, #strget
	print:
		call _print
		pop hl			; restore BASIC pointer in HL
		ret
	strget: 
		.ascii "GET"
		.db 0x0d
		.db 0x0a
		.db 0x00
	strput: 
		.ascii "PUT"
		.db 0x0d
		.db 0x0a
		.db 0x00
	__endasm;
}
void onDeviceAGON_OUTPUT ()
{
	__asm
		ld a, c
		call BIOS_CHPUT
	__endasm;
	print ("AGON OUTPUT\r\n\0");
}
void onDeviceAGON_INPUT ()
{
	print ("AGON INPUT\r\n\0");
	__asm 
		ld	hl, (_ptrHello)
		ld a, (hl)
		inc hl
		ld (_ptrHello),hl
		and a
		jr nz, nocarry
		scf
	nocarry:
	__endasm;
}
void onDeviceAGON_LOC ()
{
	print ("AGON LOC\r\n\0");
}
void onDeviceAGON_LOF ()
{
	print ("AGON LOF\r\n\0");
}
void onDeviceAGON_EOF ()
{
	print ("AGON EOF\r\n\0");
	DAC[2]=0;
	DAC[3]=0;
	VALTYP = 2; // DAC type = integer
	__asm
		ld a, #1 ; A=1
		and a ; clear carry
	__endasm;	
}
void onDeviceAGON_FPOS ()
{
	print ("AGON FPOS\r\n\0");
}
void onDeviceAGON_BACKUP_CHAR ()
{
	print ("AGON BACKUP CHAR\r\n\0");
}

#endif		// DEVICE_EXPANSION

