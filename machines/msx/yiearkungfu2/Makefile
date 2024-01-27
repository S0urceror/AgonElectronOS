BINHEX=../../../../../development/ihex-master/bin2ihex -b 0x20
ASM = ../../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld

compile: yiearkungfu2-header.bin

yiearkungfu2-header.bin: yiearkungfu2-header.asm
	$(ASM) $(ASMFLAGS) $< 

build: yiearkungfu2.hex yiearkungfu2-patched.hex
	cat yiearkungfu2-header.bin yiearkungfu2-patched.rom > yiearkungfu2.bin

yiearkungfu2.hex: yiearkungfu2.rom
	$(BINHEX) -i $< -o $@

yiearkungfu2-patched.hex: yiearkungfu2-patched.rom
	$(BINHEX) -i $< -o $@