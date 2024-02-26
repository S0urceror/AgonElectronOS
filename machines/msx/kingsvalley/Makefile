ASM = ../../../../../../MSX/Compilers/sjasm/sjasm
ASMFLAGS = $@ -s
OUTPUT_DIR = .
SOURCE_DIR = .

all: compile deploy

compile: $(OUTPUT_DIR)/kvalley.rom

$(OUTPUT_DIR)/kvalley.rom: $(OUTPUT_DIR)/kvalley.asm
	$(ASM) $< $(ASMFLAGS) 

deploy:

clean:
	rm *.rom
	rm *.lst
	rm *.sym