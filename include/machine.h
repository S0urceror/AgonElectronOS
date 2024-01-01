#ifndef __MACHINE_H_
#define __MACHINE_H_

#include "globals.h"
#include "defines.h"
#include "ff.h"

#define SLOT_0_64K_SEGMENT 0x01
#define CPM_SECTOR_SIZE 128
#define MSX_SECTOR_SIZE 512

#define VDU_SYSTEM          0
#define VDU_GP              0x80
#define VDU_MODE            0x86
#define OS_MOS              1
#define OS_ELECTRON         2
#define CTRL_W 				0x17 // 23 decimal, MOS escape code, ElectronOS 8 bits ASCII value

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
void machine_set_vsync (BOOL vsync);
void machine_vblank_handler ();
BOOL machine_set_personality (UINT8 personality);
void machine_set_vsync_address (UINT16 vsync_address);

#endif