ASM = ../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld
BINHEX=../../../../development/ihex-master/bin2ihex -b 0x20 
HEXBIN=../../../../development/ihex-master/ihex2bin 
OUTPUT_DIR = .
SOURCE_DIR = .

all: compile package

package: 
	cat $(OUTPUT_DIR)/Gulkave-header.bin "$(OUTPUT_DIR)/Gulkave (Japan).sg" $(OUTPUT_DIR)/inout.bin > $(OUTPUT_DIR)/gulkave.bin

compile: $(OUTPUT_DIR)/inout.bin $(OUTPUT_DIR)/Gulkave-header.bin 

$(OUTPUT_DIR)/inout.bin: $(OUTPUT_DIR)/inout.asm
	$(ASM) $(ASMFLAGS) $< 
$(OUTPUT_DIR)/Gulkave-header.bin: $(OUTPUT_DIR)/Gulkave-header.asm
	$(ASM) $(ASMFLAGS) $< 

hexbin:
	$(HEXBIN) -i "$(OUTPUT_DIR)/Gulkave (Japan).hex" -o "$(OUTPUT_DIR)/Gulkave (Japan).rom"

binhex:
	$(BINHEX) -i "$(OUTPUT_DIR)/Gulkave (Japan).sg" -o "$(OUTPUT_DIR)/Gulkave (Japan).hex"

clean:
	rm $(OUTPUT_DIR)/gulkave.bin