ASM = ../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld
OUTPUT_DIR = .
SOURCE_DIR = .
BINHEX=../../../../development/ihex-master/bin2ihex -b 0x20 
HEXBIN=../../../../development/ihex-master/ihex2bin 
AGON-ZDI-LOAD=../../../../development/agon-zdi-load/agon-zdi-load.py

all: compile 
	make -C kingsvalley -f Makefile

compile: $(OUTPUT_DIR)/general/bios.rom $(OUTPUT_DIR)/general/diskrom.rom $(OUTPUT_DIR)/helloworld/hello.rom $(OUTPUT_DIR)/screen2-test/test.rom $(OUTPUT_DIR)/screen2-test/testmsx.rom $(OUTPUT_DIR)/snsmat/snsmat.rom $(OUTPUT_DIR)/wrtpsg/wrtpsg.rom

$(OUTPUT_DIR)/screen2-test/test.rom: $(OUTPUT_DIR)/screen2-test/test.asm
	$(ASM) $(ASMFLAGS) -DAGONELECTRON $< 

$(OUTPUT_DIR)/screen2-test/testmsx.rom: $(OUTPUT_DIR)/screen2-test/test.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/helloworld/hello.rom: $(OUTPUT_DIR)/helloworld/helloworld.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/snsmat/snsmat.rom: $(OUTPUT_DIR)/snsmat/snsmat.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/wrtpsg/wrtpsg.rom: $(OUTPUT_DIR)/wrtpsg/wrtpsg.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/general/bios.rom: $(OUTPUT_DIR)/general/bios.asm
	$(ASM) $(ASMFLAGS) $< 

$(OUTPUT_DIR)/general/diskrom.rom: $(OUTPUT_DIR)/general/diskrom.asm $(OUTPUT_DIR)/general/driver.asm
	$(ASM) $(ASMFLAGS) $< 

deploy:
	$(BINHEX) -a 0x4fff9 -i $(OUTPUT_DIR)/general/bios.rom -o $(OUTPUT_DIR)/general/bios.hex
	$(BINHEX) -a 0x53ff9 -i $(OUTPUT_DIR)/general/diskrom.rom -o $(OUTPUT_DIR)/general/diskrom.hex
	$(BINHEX) -a 0x53ff9 -i $(OUTPUT_DIR)/helloworld/hello.rom -o $(OUTPUT_DIR)/helloworld/hello.hex
	$(BINHEX) -a 0x53ff9 -i $(OUTPUT_DIR)/screen2-test/test.rom -o $(OUTPUT_DIR)/screen2-test/test.hex

clean:
	make -C kingsvalley -f Makefile clean
	rm $(OUTPUT_DIR)/general/*.rom
	rm $(OUTPUT_DIR)/helloworld/*.rom
	rm $(OUTPUT_DIR)/screen2-test/*.rom
	rm $(OUTPUT_DIR)/general/*.lst
	rm $(OUTPUT_DIR)/helloworld/*.lst
	rm $(OUTPUT_DIR)/screen2-test/*.lst
	rm $(OUTPUT_DIR)/general/*.sld
	rm $(OUTPUT_DIR)/helloworld/*.sld
	rm $(OUTPUT_DIR)/screen2-test/*.sld
	rm $(OUTPUT_DIR)/general/*.sym
	rm $(OUTPUT_DIR)/helloworld/*.sym
	rm $(OUTPUT_DIR)/screen2-test/*.sym
	rm $(OUTPUT_DIR)/general/*.hex
	rm $(OUTPUT_DIR)/helloworld/*.hex
	rm $(OUTPUT_DIR)/screen2-test/*.hex