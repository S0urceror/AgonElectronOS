ASM = ../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld
OUTPUT_DIR = .
SOURCE_DIR = .
BINHEX=../../../../development/ihex-master/bin2ihex -b 0x20 
HEXBIN=../../../../development/ihex-master/ihex2bin 
AGON-ZDI-LOAD=../../../../development/agon-zdi-load/agon-zdi-load.py

all: compile deploy

compile: $(OUTPUT_DIR)/bios.bin $(OUTPUT_DIR)/diskrom.bin $(OUTPUT_DIR)/hello.bin $(OUTPUT_DIR)/test.bin

$(OUTPUT_DIR)/test.bin: $(OUTPUT_DIR)/test.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/hello.bin: $(OUTPUT_DIR)/helloworld.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/bios.bin: $(OUTPUT_DIR)/bios.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/diskrom.bin: $(OUTPUT_DIR)/diskrom.asm $(OUTPUT_DIR)/driver.asm
	$(ASM) $(ASMFLAGS) $< 

deploy:
	$(BINHEX) -a 0x4fff9 -i bios.bin -o bios.hex
	$(BINHEX) -a 0x57ff9 -i test.bin -o test.hex
	#cp bios.bin /Volumes/AGONLITE/msx
	#cp diskrom.bin /Volumes/AGONLITE/msx
	#cp helloworld.bin /Volumes/AGONLITE/msx
	#python3 $(AGON-ZDI-LOAD) bios.hex 0
	#python3 $(AGON-ZDI-LOAD) test.hex 0

clean:
	rm *.bin
	rm *.lst
	rm *.sld
	rm *.sym
	rm *.hex