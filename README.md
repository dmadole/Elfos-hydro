This is a loadable Elf/OS driver for the 1802/Mini compact flash card which supports DMA-driven I/O operations. This improves transfer rate from the CF card by a factor of approximately 10 over the standard BIOS IDE routines, and does not require any modification of BIOS to work.

As of build 6, there is a software workaround incorporated for latency on the DMAIN signal which could occur at high clock rates depending on the speed of the chips. It would also happen if the pull-up resistors on the processor card are higher values than the 4.7K that is recommended. At load time, the system is tested to determine if the fix is needed and it is only installed if so. On normal systems this is likely to be at around a 6-7Mhz clock with a Rev A card or a 7-8 Mhz clock with a Rev B card, and so the fix would not be installed when runing within normal specifications.

As of build 4, this driver will also work on non-DMA capable hardware such as the Pico/Elf, on which it speeds read transfers by about 2x and write transfers by about 3x. This is far less improvement than the 10x on DMA hardware but still measurable and noticeable on certain tasks. For an example of practical impact, with turbo installed, “copy zork1.dat test” runs in about 14.5 seconds with the stock BIOS driver, and in about 10.5 seconds with turbo and hydro together.

As of release 2 of hydro and releast 4 of nitro, the two programs can co-exist and utilize the same overridden BIOS vector table. This will also work with other future modules which use this same load mechanism. The communication is based on a global variable at 3CEh which has been reserved by Mike Riley for this use.

The module loads into high memory on Elf/OS and so requires kernel 0.3.1 which initializes the himem variable. It only runs in high memory and will not load into kernel memory like turbo optionally can, so it cannot coexist with applications not yet compatible with high memory, and there is no workaround. It does coexist with turbo, and the two together give a great speed improvement.

This has been tested to work on the 1802/Mini and the RC1802.

The 1802/Mini CF interface is designed for 4 Mhz operation, but as of the Rev B hardware (which A cards can be field-upgraded to) it will work up to at least 6 Mhz with "typical" chips, at least on the 1802/Mini. Results at elevated speeds may vary depending on the chips installed.

