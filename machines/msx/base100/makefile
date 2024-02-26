M80 := ~/Development/msx/Compilers/M80.net/M80
L80 := ~/Development/msx/Compilers/M80.net/L80
SJASMPLUS := ~/Development/msx/Compilers/sjasmplus/sjasmplus
SJASM := ~/Development/msx/Compilers/sjasm/sjasm
N80 := ~/Development/msx/Compilers/Nestor80/release/N80
LK80 := ~/Development/msx/Compilers/Nestor80/release/LK80
ASM = ../../../../../../MSX/Compilers/sjasmplus/sjasmplus
ASMFLAGS = --fullpath --raw=$@ --sym=$(basename $@).sym --lst=$(basename $@).lst --sld=$(basename $@).sld

HEX2BIN:= ~/Development/MSX/Compilers/Hex2bin-2.5/hex2bin

MAIN_SOURCES := BIOHDR.MAC SLOT.MAC MSXIO.MAC QUEUETL.MAC MSXGRP.MAC CASET.MAC BIO.MAC MSXCHR.MAC MSXINL.MAC \
			    BASHDR.MAC F4.MAC BINTRP.MAC MSXEXP.MAC MACLNG.MAC GENGRP.MAC ADVGRP.MAC \
				BIPTRG.MAC BIPRTU.MAC BIMISC.MAC BISTRS.MAC SPCDSK.MAC DSKCOM.MAC SPCDEV.MAC BASBIO.MAC MSXSTS.MAC INIT.MAC

MAIN_RELS := $(patsubst %.MAC,%.REL,$(MAIN_SOURCES))

HEXS := MAINROM.HEX
ROMS := $(patsubst %.HEX,%.ROM,$(HEXS))

all: $(ROMS) msx-header.bin build

$(MAIN_RELS): %.REL: %.MAC
	$(M80) -p ../basekey,../basechr =$(basename $@)
	#$(N80) $< $@ -id ../basekey -id ../basechr -bt rel -l -vb 2

$(HEXS): $(MAIN_RELS)
	$(L80) -i < MAINROM.L80
	#$(LK80) -vb 3 -of hex -o $(HEXS) $(MAIN_RELS)

$(ROMS): $(HEXS)
	$(HEX2BIN) -e ROM $(basename $@).HEX

clean: clean_compile clean_link

clean_compile:
	rm *.REL
	rm *.LST
	rm *.SYM

clean_link:
	rm *.HEX
	rm *.ROM

msx-header.bin: msx-header.asm
	$(ASM) $(ASMFLAGS) $< 	

build: 
	cat msx-header.bin mainrom.rom > mainrom.bin
	cp mainrom.bin ../../../../AgonElectron/msx/mainrom.bin