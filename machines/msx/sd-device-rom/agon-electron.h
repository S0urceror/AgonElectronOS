#ifndef AGON_ELECTRON_H__
#define AGON_ELECTRON_H__
#include <stdint.h>
#include <stdbool.h>

bool eos_machine_fopen  (uint8_t channelnumber,char* filename,uint8_t openmode);
bool eos_machine_fclose (uint8_t channelnumber);
bool eos_machine_fputc  (uint8_t channelnumber,char character);
bool eos_machine_fgetc  (uint8_t channelnumber,char* character);
bool eos_machine_feof   (uint8_t channelnumber);

#endif