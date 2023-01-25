
hydro.bin: hydro.asm include/bios.inc include/kernel.inc
	asm02 -L -b hydro.asm

clean:
	-rm -f hydro.lst
	-rm -f hydro.bin

