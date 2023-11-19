
#include "globals.h"
#include "ff.h"
#include "cp.h"
#include <Stdio.h>
#include <String.h>
#include <stdlib.h>
#include "machine.h"
#include "uart.h"

char szCommandLine[255];
char *ptrCommandLine;
void vdp_test ();
extern UINT32 clock;
void print_clock ();

// Change directory
// Parameters:
// - filename: Path of file to save
BOOL cp_CD(char *path) {
	FRESULT	fr;
	fr = f_chdir(path);
	return fr==FR_OK;
}

// Directory listing

BOOL cp_DIR(char * path) {
	FRESULT	fr;
	DIR	  	dir;
	static 	FILINFO  fno;
	int		yr, mo, da, hr, mi;
	char 	str[12];
	
	fr = f_getlabel("", str, 0);
	if(fr != 0) {
		return FALSE;
	}	
	printf("\r\nVolume: ");
	if(strlen(str) > 0) {
		printf("%s", str);
	}
	else {
		printf("<No Volume Label>");
	}
	printf("\n\r\n\r");
	
	fr = f_opendir(&dir, path);
	if(fr == FR_OK) {
		for(;;) {
			fr = f_readdir(&dir, &fno);
			if (fr != FR_OK || fno.fname[0] == 0) {
				break;  // Break on error or end of dir
			}
			yr = (fno.fdate & 0xFE00) >>  9;	// Bits 15 to  9, from 1980
			mo = (fno.fdate & 0x01E0) >>  5;	// Bits  8 to  5
			da = (fno.fdate & 0x001F);			// Bits  4 to  0
			hr = (fno.ftime & 0xF800) >> 11;	// Bits 15 to 11
			mi = (fno.ftime & 0x07E0) >>  5;	// Bits 10 to  5
			
			printf("%04d/%02d/%02d\t%02d:%02d %c %*d %s\n\r", yr + 1980, mo, da, hr, mi, fno.fattrib & AM_DIR ? 'D' : ' ', 8, fno.fsize, fno.fname);
		}
	}
	f_closedir(&dir);
	return TRUE;
}

// Make a directory
// Parameters:
// - filename: Path of file to delete
BOOL cp_MKDIR(char * filename) {
	FRESULT	fr;	
	fr = f_mkdir(filename);
	return fr==FR_OK;
}

// Delete file
// Parameters:
// - filename: Path of file to delete
BOOL cp_DEL(char * filename) {
	FRESULT	fr;	
	fr = f_unlink(filename);
	return fr==FR_OK;
}

BOOL cp_exec_batch_file (char* filename)
{
	FIL fp;
	BOOL res;
	// open file
	if (f_open(&fp, filename, ( FA_READ )) == FR_OK) 
	{
		// line by line
		while (f_gets (szCommandLine,sizeof(szCommandLine),&fp))
		{
			// process command
			res = cp_process ();
			if (res==FALSE)
				return FALSE;
		}
		f_close (&fp);
		return TRUE;
	}
	return FALSE;
}

BOOL cp_type (char* filename)
{
	FIL fp;
	BOOL res;
	// open file
	if (f_open(&fp, filename, ( FA_READ )) == FR_OK) 
	{
		// line by line
		while (f_gets (szCommandLine,sizeof(szCommandLine),&fp))
		{
			// process command
			printf ("%s\r",szCommandLine);
		}
		f_close (&fp);
		return TRUE;
	}
	return FALSE;
}

BOOL cp_process ()
{
	char  *pArg,*pEnd;
	INT16 start_address,begin,end,start,vsync_address;
	UINT8 slot = 0 ; // default slot 0
	UINT32 clock_start,clock_end;
	int i,j;
	UINT16 nr_chunks=10;
	UINT8 chunksize=8;
	UINT8 screen;
	UINT8* r;

	static const UINT8 regs_screen0[8] = {0x00,0xf0,0x00,0x00,0x01,0x00,0x00,0xe4};
	static const UINT8 regs_screen1[8] = {0x00,0xe2,0x06,0x80,0x00,0x36,0x07,0xe4};
	static const UINT8 regs_screen2[8] = {0x02,0xe2,0x0e,0x7f,0x07,0x76,0x03,0xe4};
	static const UINT8 regs_screen3[8] = {0x02,0xe2,0x0e,0x7f,0x07,0x76,0x03,0xe4};
	
	if (strstr (szCommandLine,"init")==szCommandLine)
	{
		return machine_init ();
	}	
	if (strstr (szCommandLine,"load")==szCommandLine)
	{
		pArg = szCommandLine+4;
		if (*pArg!='\0')
		{
			++pArg;
			if (pArg[0]=='-')
			{
				++pArg;
				if (pArg[0]=='w')
				{
					pArg+=2;
					return machine_set_warmboot_image (0,pArg);
				}
				if (pArg[0]>='0' && pArg[0]<='9')
				{
					slot = pArg[0]-'0';
					pArg+=2;
				}
			}
		}
		return machine_load_image (slot,pArg);
	}
	if (strstr (szCommandLine,"save")==szCommandLine)
	{
		pArg = szCommandLine+4;
		if (*pArg=='\0')
			return FALSE;
		else 
		{
			pArg++;
			begin = strtol (pArg,&pEnd,16);
			if (pEnd==NULL)
				return FALSE;
			end = strtol (pEnd,&pEnd,16);
			if (pEnd==NULL)
				return FALSE;
			start = strtol (pEnd,&pEnd,16);
			if (pEnd==NULL)
				return FALSE;
			return machine_save_image (0,begin,end,start,++pEnd);
		}
	}
	if (strstr (szCommandLine,"mount")==szCommandLine)
	{
		int secsize = MSX_SECTOR_SIZE;
		pArg = szCommandLine+5;
		if (*pArg=='\0')
			return FALSE;
		pArg++;
		if (pArg[0]=='-' && pArg[1]>='c')
		{
			pArg += 2;
			secsize = CPM_SECTOR_SIZE;
		}
		return machine_mount_disk (0,++pArg,secsize);
	}
	if (strstr (szCommandLine,"run")==szCommandLine)
	{
		start_address = 0x0000;
		pArg = szCommandLine+3;
		if (*pArg!='\0')
		{	
			pArg++;
			if (pArg[0]=='-' && pArg[1]>='0' && pArg[1]<='9')
			{
				slot = pArg[1]-'0';
				pArg+=2;
			}
			if (pArg[0]!='\0]')
				start_address = strtol (pArg,0,16);
		}
		printf ("\r\n[Personality Module active]\r\n");
		machine_start (slot,start_address);
		return TRUE;
	}
	if (strstr (szCommandLine,"vsync")==szCommandLine)
	{
		BOOL vsync = TRUE;
		pArg = szCommandLine+5;
		if (*pArg!='\0')
		{	
			pArg++;
			if (pArg[0]=='o' && pArg[1]=='f' && pArg[2]=='f')
				vsync = FALSE;
		}
		machine_set_vsync (vsync);
		return TRUE;
	}
	if (strstr (szCommandLine,"dir")==szCommandLine)
	{
		pArg = szCommandLine+3;
		if (*pArg=='\0')
			return cp_DIR (".");
		else
			return cp_DIR (++pArg);
	}
	if (strstr (szCommandLine,"cd")==szCommandLine)
	{
		pArg = szCommandLine+2;
		if (*pArg=='\0')
			return FALSE;
		else
			return cp_CD (++pArg);
	}
	if (strstr (szCommandLine,"mkdir")==szCommandLine)
	{
		pArg = szCommandLine+5;
		if (*pArg=='\0')
			return FALSE;
		else
			return cp_MKDIR (++pArg);
	}
	if (strstr (szCommandLine,"del")==szCommandLine)
	{
		pArg = szCommandLine+3;
		if (*pArg=='\0')
			return FALSE;
		else
			return cp_DEL (++pArg);
	}
	if (strstr (szCommandLine,"open")==szCommandLine)
	{
		pArg = szCommandLine+4;
		if (*pArg=='\0')
			return FALSE;
		else
			return cp_exec_batch_file (++pArg);
	}
	if (strstr (szCommandLine,"type")==szCommandLine)
	{
		pArg = szCommandLine+4;
		if (*pArg=='\0')
			return FALSE;
		else
		{
			printf ("\r\n");
			return cp_type (++pArg);
		}
	}
	if (strstr (szCommandLine,"rem")==szCommandLine)
	{
		return TRUE;
	}
	if (strstr (szCommandLine,"vdp_test")==szCommandLine)
	{
		clock_start = clock;
		for (i=0;i<256;i++)
			vdp_test ();
		clock_end = clock;
		printf ("\r\n");
		printf ("Clock start %d\r\n",clock_start);
		printf ("Clock end %d\r\n",clock_end);
		printf ("Clock diff %d\r\n",clock_end-clock_start);
		printf ("Clock diff %d seconds\r\n",(clock_end-clock_start)/100);
		printf ("I/O per seconds: %d\r\n",(256*256)/((clock_end-clock_start)/100));
		printf ("Bytes per seconds: %d",(256*256*3)/((clock_end-clock_start)/100));
		return TRUE;
	}
	if (strstr (szCommandLine,"uart_test_1")==szCommandLine)
	{
		pArg = szCommandLine+11;
		if (*pArg!='\0')
		{	
			pArg++;
			if (pArg[0]!='\0]')
				nr_chunks = strtol (pArg,&pArg,10);
			else
				return FALSE;
			if (pArg[0]!='\0]')
				chunksize = strtol (pArg,0,10);
			else
				return FALSE;
		}

		putch (0xff); // command for NULL device 127
		putch ((nr_chunks&0xff00) >> 8);
		putch (nr_chunks&0x00ff);
		putch (chunksize);
		for (i=0;i<nr_chunks;i++)
		{
			for (j=0;j<chunksize;j++)
				putch (j);
		}
		return TRUE;
	}
	if (strstr (szCommandLine,"uart_test_2")==szCommandLine)
	{
		pArg = szCommandLine+11;
		if (*pArg!='\0')
		{	
			pArg++;
			if (pArg[0]!='\0]')
				nr_chunks = strtol (pArg,&pArg,10);
			else
				return FALSE;
			if (pArg[0]!='\0]')
				chunksize = strtol (pArg,0,10);
			else
				return FALSE;
		}
		putch_direct (0xff); // command for NULL device 127
		putch_direct ((nr_chunks&0xff00) >> 8);
		putch_direct (nr_chunks&0x00ff);
		putch_direct (chunksize);
		for (i=0;i<nr_chunks;i++)
		{
			for (j=0;j<chunksize;j++)
				putch_direct (j);
		}
		return TRUE;
	}
	if (strstr (szCommandLine,"screen")==szCommandLine)
	{
		pArg = szCommandLine+6;
		if (*pArg!='\0')
		{	
			pArg++;
			if (pArg[0]!='\0]')
			{
				screen = strtol (pArg,NULL,10);
				r = regs_screen0;
				switch (screen)
				{
					case 0:
						r = regs_screen0;
						break;
					case 1:
						r = regs_screen1;
						break;
					case 2:
						r = regs_screen2;
						break;
					case 3:
						r = regs_screen3;
						break;
				}
				for (i=0;i<8;i++)
				{
					putch (0x80); // command
					putch (0x99); // port
					putch (r[i]); // value
					putch (0x80); // command
					putch (0x99); // port
					putch (0x80 | i); // value = vdp register index
				}
				return TRUE;
			}
			
		}
		
	}
	return FALSE;
}

void process_character (INT ch)
{
	if (ch>=0x20 && ch<0x7f) 
	{
		putch(ch);
		*ptrCommandLine = ch;
		ptrCommandLine++;
	}
	if (ch == '\r') // carriage return
	{
		*ptrCommandLine=0;
		ptrCommandLine=szCommandLine;
		if (cp_process ())
			printf ("\r\nOK\r\n*");
		else
			printf ("\r\nSyntax error\r\n*");
	}
	if (ch==0x08) // backspace
	{
		if (ptrCommandLine>szCommandLine)
		{
			putch (ch);
			ptrCommandLine--;
		}
	}
}
// void process_cmd (INT ch)
// {
// 	UINT8 vk, down;
// 	UINT8 cmd = ch & 0x7f; //0b01111111;
// 	switch (cmd)
// 	{
// 		case 0: // virtual key code
// 			vk = getch();
// 			down = getch();
// 			//printf ("\r\nElectronOS received virtual key %d %s\r\n",vk,down?"down":"up");
// 			machine_virtualkey (vk,down);
// 			break;
// 		default:
// 			printf ("\r\nElectronOS received unsupported command %d\r\n",cmd);
// 			break;
// 	}
// }

void cp_run ()
{
	INT ch;

	printf ("\r\n*");
	ptrCommandLine = szCommandLine;
	while (TRUE)
	{
		ch = getch();
		// if (ch & 0x80 /*0b10000000*/)
		// 	process_cmd (ch);
		// else
		// 	process_character (ch);
		process_character (ch);
	}
}