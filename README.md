This is a loadable Elf/OS driver for the 1802/Mini compact flash card which supports DMA-driven I/O operations. This improves transfer rate from the CF card by a factor of approximately 10 over the standard BIOS IDE routines, and does not require any modification of BIOS to work.

Build 10 is again a major rewrite, which splits the driver into separate initialization and run-time code, and will become the basis of the IDE driver in the next revision of MBIOS. The initialization code is much more thorough in validating drives and now displays drive information at load time. This version supports multiple controllers using port groups, allowing more than one controller to be supported in the 1802/Mini, with up to four drives supported by default. The run-time driver has been unified and made smaller, and the same driver now supports both DMA and non-DMA operation, determined dynamically.

Two options are supported on Build 10. The "-g" option specifies a list of port groups to look for controllers at, and the "-s" option does the same but first looks at the default port group before those specified. Multiple groups may be given as a list separated by commas, or the options may be repeated multiple times. After the first instance of either option, they are equivalent.

Build 9 is a major rewrite of the driver to make it work properly with two drives and to improve handling and reporting of drive errors. The detection of DMA overrun fixup is also now dynamic and is detected write-by-write and corrected only as needed. This will be more reliable in extreme borderline cases.

Builds later than 7 require kernel 0.4.0 and utilize the new heap manager which it provides.

As of build 6, there is a software workaround incorporated for latency on the DMAIN signal which could occur at high clock rates depending on the speed of the chips. It would also happen if the pull-up resistors on the processor card are higher values than the 4.7K that is recommended. At load time, the system is tested to determine if the fix is needed and it is only installed if so. On normal systems this is likely to be at around a 6-7Mhz clock with a Rev A card or a 7-8 Mhz clock with a Rev B card, and so the fix would not be installed when runing within normal specifications.

As of build 4, this driver will also work on non-DMA capable hardware such as the Pico/Elf, on which it speeds read transfers by about 2x and write transfers by about 3x. This is far less improvement than the 10x on DMA hardware but still measurable and noticeable on certain tasks. For an example of practical impact, with turbo installed, “copy zork1.dat test” runs in about 14.5 seconds with the stock BIOS driver, and in about 10.5 seconds with turbo and hydro together.

This has been tested to work on the 1802/Mini and the RC1802.

The 1802/Mini CF interface is designed for 4 Mhz operation, but as of the Rev B hardware (which A cards can be field-upgraded to) it will work up to at least 6 Mhz with "typical" chips, at least on the 1802/Mini. Results at elevated speeds may vary depending on the chips installed.

