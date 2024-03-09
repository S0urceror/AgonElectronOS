// ----------------------------------------------------------
//		callhandler.c - by Danilo Angelo, 2023
//
//		BASIC's CALL instruction extender example
//		C version
//
//      This file is only needed when extending
//      BASIC's CALL instruction and may be 
//		ignored otherwise.
// ----------------------------------------------------------

#include "applicationsettings.h"

#ifdef CALL_EXPANSION

#include "printinterface.h"
#include "agon-electron.h"
#include <stdio.h>
#include "MSX/BIOS/msxbios.h"

__at (BIOS_LINLEN) uint8_t line_length;

unsigned char onCallSDFILES(char** param) {
	// SYNTAX:
	// _SDFILES [("path")] or
	// FILES ["path"]

	char buffer[25];
	char filename[13];
	char spaced_filename[13];
	char *pfilespec;
	uint8_t i = 0;
	uint8_t column;

	buffer[0]=0;

	// process CALL statement with optional parenthesis
	if (**param == '(') 
	{
		// optional parameter supplied
		(*param)++;
		// eat any white spaces
		while (**param == ' ') {
			(*param)++;
		}
		// if not string start
		if (**param != '"') {
			return -1;
		}
		(*param)++;
		// copy everything to buffer until string end
		while ((**param != '"') && (i<sizeof(buffer)-1)) {
			buffer[i++] = *((*param)++);
		}
		buffer[i] = 0;
		// skip end double quote
		(*param)++;
		// eat any white spaces
		while (**param == ' ') {
			(*param)++;
		}
		// end parenthesis
		if (**param != ')') {
			return -1;
		}
		(*param)++;
	}
	// process without parenthesis, if any
	else if (**param == '"') 
	{
		// optional parameter supplied
		(*param)++;
		// copy everything to buffer until string end
		while ((**param != '"') && (i<sizeof(buffer)-1)) {
			buffer[i++] = *((*param)++);
		}
		buffer[i] = 0;
		// skip end double quote
		(*param)++;
	}
	// eat any white spaces
	while (**param == ' ') {
		(*param)++;
	}
	// seek end of command (0x00/EoL ou 0x3a/":")
	if ((**param != 0) && (**param != 0x3a)) {
		return -1;
	}
	// default get contents of current directory
	if (buffer[0]==0)
	{
		buffer[0] = '.';
		buffer[1] = 0;
	}
	// skip sd: if specified
	pfilespec = buffer;
	if ((buffer[0]=='s' || buffer[0]=='S') && 
		(buffer[1]=='d' || buffer[1]=='D') && 
		buffer[2]==':')
		pfilespec+=3;
	
	if (eos_machine_opendir (pfilespec))
	{
		column=0;
		while (eos_machine_readdir (filename))
		{
			//len = strlen (filename);
			sprintf (spaced_filename,"%-12s",filename);
			print (spaced_filename);
			column+=12;
			if ((column+1)<=line_length)
			{
				print (" ");
				column++;
			}
			if ((column+12)>line_length)
			{
				print ("\r\n");
				column=0;
			}
		}
		eos_machine_closedir ();
	}
	
	return 0;
}

unsigned char onCallSDCD(char** param) {
	// SYNTAX:
	// _SDFILES ("path")
	char buffer[255];
	int i = 0;

	if (**param != '(') 
		return -1;
	(*param)++;
	while (**param == ' ') {
		(*param)++;
	}
	if (**param != '"') {
		return -1;
	}
	(*param)++;
	while (**param != '"') {
		buffer[i++] = *((*param)++);
	}
	buffer[i] = 0;
	(*param)++;
	while (**param == ' ') {
		(*param)++;
	}
	if (**param != ')') {
		return -1;
	}
	(*param)++;
	// seek end of command (0x00/EoL ou 0x3a/":")
	while (**param == ' ') {
		(*param)++;
	}
	if ((**param != 0) && (**param != 0x3a)) {
		return -1;
	}

	eos_machine_chdir (buffer);
	
	return 0;
}

#endif		// CALL_EXPANSION
