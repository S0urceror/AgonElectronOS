#include "agon-electron.h"
#include "printinterface.h"

#ifdef __SDCCCALL
bool eos_machine_fopen  (uint8_t channelnumber,char* filename,uint8_t openmode)
{
    // reg A = channelnumber, reg DE = filename, stack contains openmode
    __asm
        ; from stack to reg C
        ld  bc, #0
        ld	iy, #2
        add	iy, sp
        ld	c, 0 (iy)
        ; leave rest as is
        push ix 
        ld ix, #0x001a  ; eos_machine_fopen
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_fclose (uint8_t channelnumber)
{
    // reg A = channelnumber
    __asm
        push ix 
        ld ix, #0x001c  ; eos_machine_fclose
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_fputc  (uint8_t channelnumber,char character)
{
    // reg A = channelnumber, reg L = character
    __asm
        push ix 
        ld ix, #0x001e  ; eos_machine_fputc
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_fgetc  (uint8_t channelnumber,char* character)
{
    // reg A = channelnumber, reg DE = pointer to character
    __asm
        push ix 
        ld ix, #0x0020  ; eos_machine_fgetc
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_feof   (uint8_t channelnumber)
{
    // reg A = channelnumber
    __asm
        push ix 
        ld ix, #0x0022  ; eos_machine_feof
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}

bool eos_machine_opendir (char* dirname)
{
    // reg HL = dirname
    __asm
        push ix 
        ld ix, #0x0024  ; eos_machine_opendir
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_readdir (char* filename)
{
    // reg HL = filename
    __asm
        push ix 
        ld ix, #0x0026  ; eos_machine_readdir
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;
    // return 0 or 1 in A
}
bool eos_machine_closedir ()
{
    __asm
        push ix 
        ld ix, #0x0028  ; eos_machine_closedir
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;    
    // return 0 or 1 in A
}
bool eos_machine_chdir (char* dirname)
{
    // reg HL = dirname
    __asm
        push ix 
        ld ix, #0x002a  ; eos_machine_chdir
        .db #0x5b       ; .LIL
        rst 0x38
        pop ix
    __endasm;    
    // return 0 or 1 in A
}

#endif