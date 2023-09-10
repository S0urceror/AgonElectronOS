ASM = ../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst
OUTPUT_DIR = .
SOURCE_DIR = .
BINHEX=../../../../development/ihex-master/bin2ihex -b 0x20 
HEXBIN=../../../../development/ihex-master/ihex2bin 
AGON-ZDI-LOAD=../../../../development/agon-zdi-load/agon-zdi-load.py

all: cpm deploy

cpm: $(OUTPUT_DIR)/CPM22-ccp-bdos.bin $(OUTPUT_DIR)/CPM22-bios.bin $(OUTPUT_DIR)/bios.bin

deploy:
	$(BINHEX) -a 0x4fff9 -i bios.bin -o bios.hex
	$(BINHEX) -a 0x5dbf9 -i CPM22-ccp-bdos.bin -v -o CPM22-ccp-bdos.hex
	$(BINHEX) -a 0x5f1f9 -i CPM22-bios.bin -o CPM22-bios.hex
	#cp bios.bin /Volumes/AGONLITE/cpm
	#cp CPM22-ccp-bdos.bin /Volumes/AGONLITE/cpm
	#cp CPM22-bios.bin /Volumes/AGONLITE/cpm
	#python3 $(AGON-ZDI-LOAD) bios.hex 0
	#python3 $(AGON-ZDI-LOAD) CPM22-ccp-bdos.hex 0
	#python3 $(AGON-ZDI-LOAD) CPM22-bios.hex 0

$(OUTPUT_DIR)/CPM22-ccp-bdos.bin: $(OUTPUT_DIR)/CPM22.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/CPM22-bios.bin: $(OUTPUT_DIR)/CPM22-bios.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/bios.bin: $(OUTPUT_DIR)/bios.asm
	$(ASM) $(ASMFLAGS) $< 

clean:
	rm *.bin
	rm *.lst
	rm *.sym
	rm *.hex
	