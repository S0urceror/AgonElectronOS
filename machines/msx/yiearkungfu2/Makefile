BINHEX=../../../../../development/ihex-master/bin2ihex -b 0x20

build: yiearkungfu2.hex yiearkungfu2-patched.hex

yiearkungfu2.hex: yiearkungfu2.rom
	$(BINHEX) -i $< -o $@

yiearkungfu2-patched.hex: yiearkungfu2-patched.rom
	$(BINHEX) -i $< -o $@