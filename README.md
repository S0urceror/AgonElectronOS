# Agon Electron HAL
The AgonLight is an amazing device combining retro vibes with a modern programming environment.

The Agon Electron Operating System is a EZ80 microkernel that is able to spawn different Z80 'personalities'.

## Personalities
A 'personality' is a Z80 operating environment so that it mimics a retro or fantasy machine. Currently two personalities exist:
- CP/M 2.2 machine having 64kB at it's disposal and can run CP/M DSK images (128 byte sectors)
- MSX-DOS machine that can run CP/M and MSX-DOS programs on MS-DOS compatible DSK images (512 byte sectors, 9 sectors, 80 tracks, 2 sides).

## Commands
Currently the OS has the following commands:

### INIT
Initialises a personality / Z80 operating environment. This is a mandatory first step before we can RUN a personality.

### LOAD
Load a binary image into memory. Binary images have a header that presedes the real code that specifies it's location in memory and start address. Check MSX binary format to get an idea how this works.
* -w specifies a warmboot image that is reloaded when requested, needed for CP/M.
* -0 to -3 specifies the 'slot' the image is loaded in. 0 is 0x50000 and 1 is 0x60000, etc.
### SAVE
Saves a binary image from memory in slot 0. This also creates the header as mentioned before.
* [begin address]
* [end address]
* [start address]
### MOUNT
Mount a DSK image to function as a virtual floppy drive.
* -c to specify CP/M sector size, otherwise 512kB sectors are presumed.
* [filename]
### RUN
Run the personality from the specified slot and starting address
* -0 to -3 specify the 'slot' where the Z80 is starting. 0 when omitted.
* [start address]
### DIR
Show a directory from the SD card.
* [subdirectory] or the current directory when omitted.

### CD
Change directory to.
* [subdirectory]
### MKDIR
Make directory.
* [subdirectory]
### DEL
Delete a file or empty directory
* [filename/directoryname]
### TYPE
Type a file contents to the screen
* [filename]
### OPEN
Open and run the contents of a batch file
* [filename]

## Batch files
The batch files contain a collection of abovementioned commands and can be used to automate things.

### CP/M example
```
init
load /cpm/bios.bin
load -w /cpm/CPM22-ccp-bdos.bin
load /cpm/CPM22-bios.bin
mount -c /cpm/floppy.dsk
run 0x0000
```

### MSX example
The bios is loaded in both slot 0 and slot 1 to avoid some interslot call issues. We have plenty of RAM on the Agon so that's not an issue. We have slot 0 identifying as ROM and slot 1 identify as RAM so that's what MSX-DOS will use to load MSXDOS.SYS and COMMAND.COM. This will overwrite the bios in slot 1 in the end but the diskrom is remaining. The EZ80 internal RAM is mapped into HIMEM from E000 to FFFF and is remapped on slot-switches so the important MSX page 3 is seemingly always there.
```
init
load -0 /msx/bios.bin
load -1 /msx/bios.bin
load -1 /msx/diskrom.bin
mount /msx/MSXDOS.DSK
run -0 0000
```