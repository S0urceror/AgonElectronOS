PYTHON=python3
BINHEX=../../../../../development/ihex-master/bin2ihex -b 0x20
ASM = ../../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld

all: compile deploy

header.bin: header.asm
	$(ASM) $(ASMFLAGS) $< 

compile: 
	$(PYTHON) MAKE/make.py Debug

clean:
	$(PYTHON) MAKE/make.py Debug clean

deploy: header.bin
	cat header.bin Debug/bin/sd-device.ROM > Debug/bin/sd-device.bin