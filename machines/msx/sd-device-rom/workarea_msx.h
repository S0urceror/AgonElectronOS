#ifndef __WORKAREA_MSX_H_
#define __WORKAREA_MSX_H_

#include "workarea.h"

extern const uint8_t byte_in_rom_space;

void allocate_workarea (uint16_t size) __z88dk_fastcall __naked;
void init_workarea ();
void set_workarea (WORKAREA* wrk) __z88dk_fastcall __naked;
WORKAREA* get_workarea() __z88dk_fastcall __naked;
uint8_t get_slot_id (uint16_t address)  __z88dk_fastcall __naked;

#endif // __WORKAREA_MSX_H_