This is a loadable Elf/OS driver for the 1802/Mini compact flash card which supports DMA-driven I/O operations. This improves transfer rate from the CF card by a factor of approximately 10 over the standard BIOS IDE routines, and does not require any modification of BIOS to work.

This driver uses the same BIOS vector table override mechanism that I developed as part of nitro, however the two do not know how to cooperate in this, so they cannot both be loaded at the same time. I have plans to fix this so that multiple modules that need this capability can be loaded at the same time.

The module loads into high memory on Elf/OS and so requires kernel 0.3.1 which initializes the himem variable. It only runs in high memory and will not load into kernel memory like turbo optionally can, so it cannot coexist with applications not yet compatible with high memory, and there is no workaround. It does coexist with turbo, and the two together give a great speed improvement.

