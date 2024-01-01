
all:
	make -C machines/msx -f Makefile
	make -C machines/cpm -f Makefile
	make -C machines/sg1000 -f Makefile

clean:
	make -C machines/msx -f Makefile clean
	make -C machines/cpm -f Makefile clean
	make -C machines/sg1000 -f Makefile clean