// ----------------------------------------------------------
//		msxromapp.c - by Danilo Angelo, 2020-2023
//
//		ROM program(cartridge) for MSX example
//		C version
// ----------------------------------------------------------

#include <stdint.h>
#include <stdio.h>

#include "MSX/BIOS/msxbios.h"
#include "targetconfig.h"
#include "applicationsettings.h"
#include "printinterface.h"
#include "MSX/BIOS/msxbios.h"
#include "workarea_msx.h"

__at (BIOS_H_FILE) uint8_t h_files[];


void files (char* param)
{
	// do not return back to caller
	// BASIC BIOS assumes we are not returning and throw a 'illegal function call'
	// replacing with address that is guaranteed a RET (trick from diskrom)
	__asm
		push de ; save DE
		; store HL in WORKAREA
		ex de,hl
		push de
		call _get_workarea
		pop de
		push hl ; save pointer to workarea
		; store basic pointer in workarea
		ld (hl),e
		inc hl
		ld (hl),d
		; replace return address
		ld hl,#4+6 ; DE+HL = 4, return address to be replace at position 6
		add hl,sp
		ld (hl),#0x8b
		inc hl
		ld (hl),#0xf3
		pop hl ; restore pointer to workarea
		pop de
		push hl
		call _onCallSDFILES
		pop hl
		; return updated basic pointer
		ld a, (hl)
		inc hl
		ld h, (hl)
		ld l, a
	__endasm;
}

void print_welcome_msg (uint16_t work_area_size, char hook_indicator)
{
	char szLine[40];
	sprintf (szLine, "Electron BASIC v0.2.%x.%c\r\n",work_area_size,hook_indicator);
	print(szLine);
}

// ----------------------------------------------------------
//	This is the main function for your C MSX APP!
//
//	Your fun starts here!!!
//	Replace the code below with your art.
void main(void) 
{
	uint16_t function_address;
	uint16_t work_area_size;

	// allocate TSR memory
	// this moves and copies the stack
	// if this main function also uses the stack for variables the program will crash
	work_area_size = sizeof (WORKAREA);
    allocate_workarea (work_area_size);
    // initialize
    init_workarea();

	if (h_files[0]!=0xc9)
	{
		// diskrom is active
		print_welcome_msg (work_area_size,'c');
	}
	else
	{
		// diskrom is inactive
		// welcome message not visible
		// print_welcome_msg (work_area_size,'f');

		// replace H.FILE handler to point to files
		function_address = (uint16_t) &files;
		uint8_t slot_id = get_slot_id ((uint16_t) &byte_in_rom_space);
		h_files[0]=0xf7; // RST30h
		h_files[1]=slot_id;
		h_files[2]=function_address & 0xff;
		h_files[3]=function_address >> 8;
		h_files[4]=0xc9; // ret
	}
}

