ASM = ../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst
OUTPUT_DIR = .
SOURCE_DIR = .
BINHEX=../../../../development/ihex-master/bin2ihex -b 0x20 
HEXBIN=../../../../development/ihex-master/ihex2bin 
AGON-ZDI-LOAD=../../../../development/agon-zdi-load/agon-zdi-load.py

all: compile deploy

compile: $(OUTPUT_DIR)/bios.bin $(OUTPUT_DIR)/diskrom.bin $(OUTPUT_DIR)/helloworld.bin

$(OUTPUT_DIR)/helloworld.bin: $(OUTPUT_DIR)/helloworld.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/bios.bin: $(OUTPUT_DIR)/bios.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/diskrom.bin: $(OUTPUT_DIR)/diskrom.asm $(OUTPUT_DIR)/driver.asm
	$(ASM) $(ASMFLAGS) $< 

deploy:
	cp bios.bin /Volumes/AGONLITE/msx
	cp diskrom.bin /Volumes/AGONLITE/msx
	cp helloworld.bin /Volumes/AGONLITE/msx
	#$(BINHEX) -a 0x4fff9 -i bios.bin -o bios.hex
	#python3 $(AGON-ZDI-LOAD) bios.hex 0

clean:
	rm *.bin