extern char coldBoot;		// 1 = cold boot, 0 = warm boot

#define	OS_major		0
#define	OS_minor		8
#define	OS_revision 	3

#define ESC 0x1b
#define CTRL_Y 0x19
#define CTRL_Z 0x1a

#ifndef NULL
#define NULL	(void *)0	//!< The NULL definition.
#endif
#ifndef TRUE
#define TRUE	1		//!< The TRUE definition.
#endif
#ifndef FALSE
#define FALSE	0		//!< The FALSE definition.
#endif