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

extern void romInit(void);

// ----------------------------------------------------------
//	This is a parameterized CALL handler example.
//	CALL CMD1 (<STRING>)
//	return	0: Success; anything else: syntax error
//
//	This is only for the demo app.
//	To disable the support for BASIC's CALL statement:
//	1) Set CALL_EXPANSION to _OFF in ApplicationSettings.txt
//	To completely remove the support for BASIC's CALL statement from the project:
//	1) Set CALL_EXPANSION to _OFF in ApplicationSettings.txt
//	2) Optionally, remove/comment all CALL_STATEMENT items in ApplicationSettings.txt
//	3) Remove all onCallXXXXX functions from this file
unsigned char onCallSDFILES(char** param) {
	// SYNTAX:
	// _SDFILES [("path")]

	char buffer[255];
	int i = 0;

	buffer[0]=0;
	
	if (**param == '(') 
	{
		// optional parameter supplied
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
	}
	// seek end of command (0x00/EoL ou 0x3a/":")
	while (**param == ' ') {
		(*param)++;
	}
	if ((**param != 0) && (**param != 0x3a)) {
		return -1;
	}
	print("Listing files:\0");
	if (buffer[0]==0)
		print ("*.*");
	else
		print(buffer);
	print("\r\n\0");
	
	return 0;
}

// ----------------------------------------------------------
//	This is a parameterless CALL handler example.
//	CALL RUNCART
//	return	0: Success; anything else: syntax error
// 
//	This is only for the demo app.
//	To disable the support for BASIC's CALL statement:
//	1) Set CALL_EXPANSION to _OFF in ApplicationSettings.txt
//	To completely remove the support for BASIC's CALL statement from the project:
//	1) Set CALL_EXPANSION to _OFF in ApplicationSettings.txt
//	2) Optionally, remove/comment all CALL_STATEMENT items in ApplicationSettings.txt
//	3) Remove all onCallXXXXX functions from this file
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

	print ("CD: \0");
	print (buffer);
	print("\r\n\0");
	
	return 0;
}

#endif		// CALL_EXPANSION
