#ifndef __MACHINE_H_
#define __MACHINE_H_

#include "globals.h"
#include "defines.h"
#include "ff.h"


#define CPM_SECTOR_SIZE 128
#define MSX_SECTOR_SIZE 512

struct HEADER
{
	UINT8  magic;
	UINT16 begin;
	UINT16 end;
	UINT16 start;
};

BOOL machine_load_image (UINT8 bank, TCHAR* filename);
BOOL machine_save_image (UINT8 bank, UINT16 begin, UINT16 end, UINT16 start, TCHAR* filename);
BOOL machine_mount_disk (UINT8 drive_nr, TCHAR* filename,int secsize);
BOOL machine_init ();

void machine_start (UINT8 bank,UINT16 start_address);
void machine_warm_boot ();
BOOL machine_set_warmboot_image (UINT8 bank, char* szFilename);

UINT16 machine_read_write_disk (UINT16 mb, UINT16 af, UINT16 bc, UINT16 de, UINT16 hl);

#endif