#include <eZ80.h>
#include <defines.h>
#include <stdio.h>
#include <stdlib.h>
#include "machine.h"
#include "misc.h"
#include <String.h>
#include "vectors16.h"
#include "uart.h"
#include "timer.h"

INT8 *ram;
INT8  mbase;
FIL disks[8]; // A-H
FIL channels[7]; // #1 to #8
UINT8 channel_state;
char szWarmBootImage[255];
UINT8 wb_bank;
int _secsize;
UINT32 machine_vsync_address;
UINT8 machine_vsync_register;
UINT8 machine_vsync_running;
BOOL want_vsync;
DIR* current_dir;

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
	ram = (INT8*) (0x10000 * SLOT_0_64K_SEGMENT);
	mbase = SLOT_0_64K_SEGMENT;
	RAM_ADDR_U = mbase; // MSM: map internal ram to upper 4k of selected 64kB
	machine_vsync_address=0x0000;
	machine_vsync_register=0;
	machine_vsync_running=0;
	want_vsync = FALSE;
	memset (disks,0,sizeof (disks));
	memset (channels,0,sizeof (channels));
	channel_state = 0; // 0b00000000;
	current_dir=NULL;

	// // set port C bit 0 to output, L=GND, H=Vcc
	// // leave rest set to input mode
	// PC_DDR = 0xfe;  //0b11111110;
	// PC_ALT1 = 0x00; //0b00000000;
	// PC_ALT2 = 0x00; //0b00000000;
	// // set bit0 to 0
	// PC_DR = PC_DR & 0xfe; //0b11111110;

	return TRUE;
}

void machine_start (UINT8 bank,UINT16 start_address)
{
	if (want_vsync==TRUE)
		set_vector(PORTB1_IVECT, machine_vblank_handler); 	// 0x32
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
	UINT bytes_read,bytes_written;
	INT8* sectorbuffer;
	UINT8 mb = (mbase>>8)-SLOT_0_64K_SEGMENT;
	UINT8 a = af>>8;
	UINT8 b = bc>>8;
	BOOL carry = af & 1;

	/*
	const char drive_letter[] = "ABCDEFGH";
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
	want_vsync = vsync;
}

extern UINT32 clock;
UINT32 prev_clock;
void print_clock ()
{
	printf ("Clock: %d (%d)\r\n",clock,clock - prev_clock);
	prev_clock = clock;
}

BOOL machine_set_personality (UINT8 personality)
{
	UINT8 ch=0;
	int i;
	putch(CTRL_W);						
	putch(VDU_SYSTEM);
	putch(VDU_GP);
	putch(personality);
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
			if (getch()==personality)
				return TRUE;
		}
	}
	return FALSE;
}

void machine_set_vsync_address (UINT16 address)
{
	machine_vsync_address = address;	
}

void machine_print (char* szBuf)
{
	char* p=szBuf;
	while (*p!='\0')
	{
		machine_putc (*p);
		p++;
	}
}

char machine_fopen (UINT8 channel,char* filename,UINT8 openmode)
{
	FRESULT f;
	BYTE mode=FA_READ;
	char szBuf[40];

	// zero-based channel number
	channel--;
	
	// invalid openmode
	if (openmode==0)
		return 0;

	// convert machine openmode into FATFS openmode
	switch (openmode)
	{
		case 1: mode=FA_READ;
				break;
		case 2: mode=FA_CREATE_ALWAYS|FA_WRITE;
				break;
		case 4: mode=FA_READ | FA_WRITE;
				break;
		case 8: mode=FA_OPEN_APPEND | FA_WRITE;
				break;
	}
	// sprintf (szBuf,"mode: %x\r\n",mode);
	// machine_print (szBuf);

	if (channel_state & (1<<channel) > 0)
		return 0; // cannot reopen
	
	f = f_open (&channels[channel],filename,mode);
	if (f==FR_OK)
	{
		// sprintf (szBuf,"fpos: %d-%d\r\n",channels[channel].fptr,channels[channel].obj.objsize);
		// machine_print (szBuf);
		channel_state = channel_state | (1<<channel);
		return 1;
	}

	return 0;
}

char machine_fclose (UINT8 channel)
{
	FRESULT f;

	// zero-based channel number
	channel--;

	// check file open state
	if (channel_state & (1<<channel) == 0)
		return 0; // cannot close something that is not open

	f = f_close (&channels[channel]);

	if (f==FR_OK)
	{
		// clear file channel entry
		channel_state = channel_state & (~(1<<channel));
		return 1;
	}
	return 0;
}

char machine_fgetc (UINT8 channel, char* character)
{
	FRESULT f;
	UINT br;

	// zero-based channel number
	channel--;

	// check file open state
	if (channel_state & (1<<channel) == 0)
		return 0; // cannot get characters from closed file

	f = f_read (&channels[channel],character,1,&br);

	if (f==FR_OK && br==1)
		return 1;
	return 0;
}

char machine_fputc (UINT8 channel,UINT8 character)
{
	// zero-based channel number
	channel--;

	// check file open state
	if (channel_state & (1<<channel) == 0)
		return 0; // cannot put characters to closed file

	if (f_putc (character,&channels[channel])>0)
		return 1;
	else
		return 0;
}

char machine_feof (UINT8 channel)
{
	channel--;
	if (channel_state & (1<<channel) == 0)
		return 1; // cannot get eof status from closed file

	return f_eof (&channels[channel]);
}

char machine_opendir (char* path)
{
	if (current_dir)
		return 0; // error, close the previous directory first

	current_dir = malloc (sizeof (DIR));
	if(f_opendir(current_dir, path) == FR_OK)
		return 1;
	return 0;
}
char machine_readdir (char* filename)
{
	FILINFO fno;
	char* filename_to_copy;
	UINT8 max = 13;
	UINT8 i;

	if (!current_dir)
		return 0; // not an open dir.

	if (f_readdir (current_dir,&fno) != FR_OK)
		return 0; // error
	if (fno.fname[0] == 0)
		return 0; // end of directory


	if (fno.altname[0])
		filename_to_copy = fno.altname;
	else
		filename_to_copy = fno.fname;

	// maximum 13 chars
	if (fno.fattrib & AM_DIR) 
	{
		*(filename++)='[';
		max--;
	}
	for (i=0;i<max;i++)
	{
		if (filename_to_copy[i]!=0)
			*(filename++) = filename_to_copy[i];
		else
			break;
	}
	if (fno.fattrib & AM_DIR) 
		*(filename++)=']';
	*(filename++)='\0';
	
	return 1;
}
char machine_closedir ()
{
	if (!current_dir)
		return 0; // not an open dir.

	f_closedir (current_dir);

	free (current_dir);
	current_dir = NULL;

	return 1;
}
char machine_chdir (char* path)
{
	return (f_chdir(path)==FR_OK);
}