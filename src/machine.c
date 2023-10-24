#include <eZ80.h>
#include <defines.h>
#include <stdio.h>
#include "machine.h"
#include "misc.h"
#include <String.h>
#include "vectors16.h"

INT8 *ram;
INT8  mbase;
FIL disks[8]; // A-H
char szWarmBootImage[255];
UINT8 wb_bank;
int _secsize;
UINT32 machine_vsync_address;
UINT8 machine_vsync_register;
UINT8 machine_vsync_running;

BOOL machine_load_image (UINT8 bank,TCHAR* filename)
{
	FIL f;
	UINT bytes_read;
	struct HEADER header;

	if (f_open(&f, filename, ( FA_READ )) == FR_OK) 
	{
		if (f_read (&f,&header,sizeof(header),&bytes_read)==FR_OK)
		{
			f_read (&f,&ram[header.begin+bank*0x10000],header.end-header.begin,&bytes_read);
			//printf ("[%s loaded]\r\n",filename);
		}
		f_close (&f);
		return TRUE;
	}
	else
	{
		//printf ("[%s NOT loaded]\r\n",filename);
		return FALSE;
	}
}

BOOL machine_save_image (UINT8 bank, UINT16 begin, UINT16 end, UINT16 start, TCHAR* filename)
{
	FIL f;
	UINT bytes_written;
	struct HEADER header;

	if (f_open(&f, filename, ( FA_CREATE_NEW | FA_WRITE )) == FR_OK) 
	{
		header.start = start;
		header.begin = begin;
		header.end = end;
		header.magic = 0xaa;

		if (f_write (&f,&header,sizeof(header),&bytes_written)==FR_OK)
		{
			f_write (&f,&ram[header.begin+bank*0x10000],header.end-header.begin,&bytes_written);
			//printf ("[%s saved from %04X,%04X,%04X]\r\n",filename,begin,end,start);
		}
		f_close (&f);
		return TRUE;
	}
	else
	{
		//printf ("[%s NOT loaded]\r\n",filename);
		return FALSE;
	}
}

BOOL machine_mount_disk (UINT8 drive_nr, TCHAR* filename,int secsize)
{
	const char drive_letter[] = "ABCDEFGH";
	if (f_open(&disks[drive_nr], filename, ( FA_READ | FA_WRITE )) == FR_OK) 
	{
		//printf ("[%s mounted as disk %c:]\r\n",filename,drive_letter[drive_nr]);
		_secsize = secsize;
		return TRUE;
	}
	else
	{
		//printf ("[%s NOT mounted]\r\n",filename);
		return FALSE;
	}
}

BOOL machine_init ()
{
	int i;
	ram = (INT8*) 0x050000;
	mbase = 0x05;
	RAM_ADDR_U = mbase; // MSM: map internal ram to upper 4k of selected 64kB
	machine_vsync_address=0x0000;
	machine_vsync_register=0;
	machine_vsync_running=0;

	// set port C bit 0 to output, L=GND, H=Vcc
	// leave rest set to input mode
	PC_DDR = 0xfe;  //0b11111110;
	PC_ALT1 = 0x00; //0b00000000;
	PC_ALT2 = 0x00; //0b00000000;
	// set bit0 to 0
	PC_DR = PC_DR & 0xfe; //0b11111110;

	return TRUE;
}

void machine_start (UINT8 bank,UINT16 start_address)
{
	exec16((UINT24)(ram+start_address+bank*0x10000), NULL); 
}

BOOL machine_set_warmboot_image (UINT8 bank,char* szFilename)
{
	wb_bank = bank;
	strcpy (szWarmBootImage,szFilename);
	return TRUE;
}
void machine_warm_boot ()
{
	machine_load_image (wb_bank,szWarmBootImage);
}

UINT16 machine_read_write_disk (UINT16 mbase,UINT16 af, UINT16 bc, UINT16 de, UINT16 hl)
{
	const char drive_letter[] = "ABCDEFGH";
	UINT bytes_read,bytes_written;
	INT8* sectorbuffer;
	UINT8 mb = (mbase>>8)-5;
	UINT8 a = af>>8;
	UINT8 b = bc>>8;
	BOOL carry = af & 1;

	/*
	printf ("%04x %04x %04x %04x\r\n",af,bc,de,hl);
	printf ("%s %d bytes from location %d into memory location %04X from disk %c\r\n",
		carry?"Written":"Read",
		b*_secsize,
		de*_secsize,
		hl,
		drive_letter[a]);
	*/

	if (f_lseek(&disks[a], de*_secsize) != FR_OK)
        return 0x0101; // return A=1 + carry to signal error

	sectorbuffer = ram + hl + mb*0x10000;
	if (carry==FALSE)
	{
		// read
		if (f_read(&disks[a], sectorbuffer, b*_secsize, &bytes_read) != FR_OK)
			return 0x0101; // return A=1 + carry to signal error
	}
	else
	{
		// write
		if (f_write(&disks[a], sectorbuffer, b*_secsize, &bytes_written) != FR_OK)
			return 0x0101; // return A=1 + carry to signal error
	}
	return 0x0000; // no error-code, no carry
}

void machine_set_vsync (BOOL vsync)
{
	if (vsync==TRUE)
		set_vector(PORTB1_IVECT, machine_vblank_handler); 	// 0x32
}

extern UINT32 clock;
UINT32 prev_clock;
void print_clock ()
{
	printf ("Clock: %d (%d)\r\n",clock,clock - prev_clock);
	prev_clock = clock;
}