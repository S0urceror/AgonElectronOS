ASM = ../../../../../../MSX/Compilers/sjasm/sjasm
ASMFLAGS = $@ -s
OUTPUT_DIR = .
SOURCE_DIR = .

all: compile deploy

compile: $(OUTPUT_DIR)/kvalley.bin $(OUTPUT_DIR)/kvalley.rom

$(OUTPUT_DIR)/kvalley.bin: $(OUTPUT_DIR)/kvalley.asm
	$(ASM) $< $(ASMFLAGS) 
$(OUTPUT_DIR)/kvalley.rom: $(OUTPUT_DIR)/kvalley.asm
	$(ASM) $< $(ASMFLAGS) 

deploy:
	cp kvalley.bin /Volumes/AGONLITE/msx

clean:
	rm *.bin
	rm *.rom
	rm *.lst
	rm *.sym