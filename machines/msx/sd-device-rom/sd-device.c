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
#include "agon-electron.h"
#include "MSX/BIOS/msxbios.h"
#include <stdint.h>

__at (BIOS_DEVICE) char req_device;
__at (BIOS_FILNAM) char filename[];
__at (BIOS_FILNM2) char filename2;
__at (BIOS_PTRFIL) uint16_t ptrfil;
__at (BIOS_DAC) uint8_t DAC[];
__at (BIOS_VALTYP) uint16_t VALTYP;

// SNERR   EQU     4055H                   ; syntax error
// FCERR   EQU     475AH                   ; illegal function call error
// USERR   EQU     481CH                   ; undefined line number error
// TMERR   EQU     406DH                   ; type mismatch error
// DIOERR  EQU     73B2H                   ; device I/O error
// DERFAO  EQU     6E6EH                   ; file already open error
// DERFNO  EQU     6E77H                   ; file not open error
// DERIER  EQU     6E80H                   ; internal error
// DERSOO  EQU     6E86H                   ; sequential I/O only error
// DERBFN  EQU     6E6BH                   ; bad filename error

uint8_t get_channelnumber ()
{
	return DAC[2];
}
void throw_error (uint8_t error)
{
	// assume SDCC_CALL
	#ifdef __SDCCCALL
	__asm
		ld 		e, a
		ld      ix, #0x408B ; #0x406F ;ERROR
		ld		iy,(#BIOS_EXPTBL+0-1)
		jp		BIOS_CALSLT
	__endasm;
	#endif
}

void onDeviceSD_INIT() 
{
}

// DEVICE OPEN
// Input:
//   HL = i/o channel pointer, D = device code, E = file open mode
//   FILNAM = filename requested
//   DAC+2  = BASIC channel number
// Output:
//   success: (PTRFIL)    = HL i/o channel pointer
//            (PTRFIL)[0] = file open mode
//   failure: call ERROR in ROM with appropiate error number in E
//
// i/o channel buffer contains:
// 00 - file open mode
// 01 - pointer to FCB - we use this for channelnumber
// 02 - pointer to FCB
// 03 - reserved
// 04 - device code, same as D register
void onDeviceSD_OPEN (uint16_t reghl,uint16_t regde)
{
	char* p = (char*) reghl;
	char filename_ext[12];
	uint8_t regE = (uint8_t)(regde&0xff);
	uint8_t openmode = regE;
	uint8_t channelnumber = get_channelnumber ();

	if (openmode==4)
		throw_error (58); // DERSOO, serial only

	// convert a filename from this: "ABC     TXT" to this: "ABC.TXT"
	char* pfilename=filename_ext;
	uint8_t i;
	for (i=0;i<8;i++)
	{
		if (filename[i]!=' ')
			*pfilename++=filename[i];
		else
			break;
	}
	*pfilename++='.';
	for (i=0;i<3;i++)
	{
		if (filename[i+8]!=' ')
			*pfilename++=filename[i+8];
		else
			break;
	}
	*pfilename++='\0';	

	if (eos_machine_fopen (channelnumber,filename_ext,openmode))
	{
		// at success:
		// (HL) should receive requested open mode in E
		// (PTRFIL) should point to buffer passed hl
		p[0] = regE; 			// store openmode at index 0
		p[1] = channelnumber; 	// store channelnumber at index 1
		ptrfil = reghl;
	}
	else
	{
		throw_error (56); // DERBFN
	}
}
// DEVICE CLOSE
// Input:
//   HL = i/o channel pointer
// Output:
//   success: *(HL) = 0, changes open mode to closed (0)
//   failure: call ERROR in ROM with appropiate error number in E
void onDeviceSD_CLOSE (uint16_t reghl)
{
	char* p = (char*) reghl;

	uint8_t channelnumber = p[1];// get_channelnumber ();
	if (eos_machine_fclose (channelnumber))
		*p = 0;
	else
		throw_error (56); // DERBFN
}

// DEVICE RANDOM ACCESS (GET/PUT)
// Input:
//  HL: filebuffer
//  DE: BASIC pointer
//  B: 00h = GET, 80h = PUT
// Output:
//  HL: BASIC pointer (updated)
void onDeviceSD_RANDOM_ACCESS ()
{
	throw_error (56); // DERBFN
	//print ("SD RANDOM ACCESS\r\n\0");
}

// DEVICE OUTPUT
// Input:
//   HL = i/o channel pointer, C = character
// Output:
//   failure: call ERROR in ROM with appropiate error number in E
void onDeviceSD_OUTPUT (uint16_t reghl)
{
	uint8_t regc;
	char* p = (char*) reghl;
	// do this first before register C is destroyed
	__asm
		; put register C in space reserved for uint8_t regc
		ld	iy, #0
		add	iy, sp
		ld	0 (iy),c
	__endasm;

	if (*p!=2 && *p!=8) // not output or append mode?
	{
		throw_error (0x3d);
		return;
	}

	uint8_t channelnumber = p[1]; // get_channelnumber ();
	if (eos_machine_fputc (channelnumber,regc))
	{

	}
	else
	{
		throw_error (19); // Device IO error
	}
}

// DEVICE INPUT
// Input:
//   HL = i/o channel pointer
// Output:
//   success: A=char, not zero and no carry
//   failure: carry flag, call ERROR in ROM with appropiate error number in E
void onDeviceSD_INPUT (uint16_t reghl)
{
	char* p = (char*) reghl;
	char character;
	uint8_t channelnumber = p[1]; // get_channelnumber ();

	if (eos_machine_fgetc (channelnumber,&character))
	{
		__asm
			ld a, (de) ; register DE still points to address of character
			and a
		__endasm;
	}
	else
	{
		__asm 
			scf
		__endasm;
	}
}

// LOC()
// for a random record file: the record number that has been accessed last with GET or PUT. 
// If no record has been accessed, number "0" will be returned.
// for a sequential file: the number of bytes that have been read to the buffer or written from buffer to the disk. 
// In writing mode, it will return the value 0 when you don't have yet written 256 bytes.
void onDeviceSD_LOC ()
{
	print ("SD LOC\r\n\0");
	// return bytes received/send
	// put integer result in DAC
}

// LOF()
// Returns the size of a file on disk in bytes.
void onDeviceSD_LOF ()
{
	// return bytes free in receive buffer
	// put integer result in DAC
	print ("SD LOF\r\n\0");
}

// DEVICE EOF
// Input
//  HL = i/o channel pointer
// Output
//  DAC set to -1 (eof) or 0 (not eof)
//  A = 1 (eof) or 0 (not eof)
//  Carry = 1 (eof) or 0 (not eof)
void onDeviceSD_EOF (uint16_t reghl)
{
	char* p = (char*) reghl;
	uint8_t channelnumber = p[1]; // get_channelnumber ();

	// return with A=0 for EOF
	// C=0 for not EOF
	// also put result in DAC and VALTYP
	if (!eos_machine_feof(channelnumber))
	{
		// not end-of-file
		DAC[2]=0;
		DAC[3]=0;
		VALTYP = 2; // DAC type = integer
		__asm
			ld a, #1 ; A=1
			and a ; clear carry
		__endasm;
		return;
	}
	// C=1 => ERROR or EOF
	DAC[2]=255;
	DAC[3]=255;
	VALTYP = 2; // DAC type = integer
	__asm 
		xor a ; A=0
		scf ; set carry
	__endasm;
}

// FPOS() is a reserved word that has never been used.
// This function was meant to return physical sector of where opened file number is located. 
// Since in FAT file system cluster size can differ and disk can be fragmented this was probably 
// left out to avoid problems when used without other needed support functions.
void onDeviceSD_FPOS ()
{
	print ("SD FPOS\r\n\0");
	throw_error (5); // FCERR
}

// HL = i/o channel pointer, C = character
void onDeviceSD_BACKUP_CHAR ()
{
	__asm
		ld a, c
		call BIOS_CHPUT
	__endasm;
	print ("SD BACKUP CHAR\r\n\0");
}

#endif		// DEVICE_EXPANSION

