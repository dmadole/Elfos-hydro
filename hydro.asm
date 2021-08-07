
;  Copyright 2021, David S. Madole <david@madole.net>
;
;  This program is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  This program is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program.  If not, see <https://www.gnu.org/licenses/>.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc


           ; Define non-published API elements

d_ideread  equ     0447h
d_idewrite equ     044ah


           ; Hardware port definitions

#define    cf_addr 2
#define    cf_data 3


           ; Bits in CF interface address port

cf_count   equ     80h                 ; dma sector count
cf_dmout   equ     40h                 ; dma out enable
cf_dmain   equ     20h                 ; dma in enable
cf_inten   equ     10h                 ; interrupt enable


           ; IDE register addresses

ide_data   equ     0
ide_erro   equ     1
ide_coun   equ     2
ide_sect   equ     3
ide_cyll   equ     4
ide_cylh   equ     5
ide_head   equ     6
ide_stat   equ     7
ide_cmnd   equ     7
ide_alts   equ     14
ide_cont   equ     14


           ; Bits in IDE status register (address 7)

ide_bsy    equ     80h                 ; busy
ide_rdy    equ     40h                 ; ready
ide_drq    equ     08h                 ; data request
ide_err    equ     01h                 ; error


           ; IDE command code values

ide_read   equ     20h                 ; read sector
ide_writ   equ     30h                 ; write sector


           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     br      entry


           ; Build information

           db      8+80h              ; month
           db      6                  ; day
           dw      2021               ; year
           dw      0                  ; build

           db      'See github.com/dmadole/Elfos-hydro for more info',0


           ; Check if hook points have already been patched and do not
           ; install if so, since we don't know what it is or what the
           ; impact might be of disconnecting it.

entry:     ldi     high patchpio      ; Get point to table of patch points
           phi     rd
           ldi     low patchpio
           plo     rd

chekloop   lda     rd                 ; a zero marks end of the table
           lbz     chekvers

           phi     rf                 ; get pointer to patch point
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           ldn     rd                 ; if points into bios then ok
           smi     0f8h
           lbnf    cheknext

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Read or write hooks already installed',13,10,0
           sep     sret

cheknext:  inc     rd                 ; skip target address in table
           inc     rd

           lbr     chekloop           ; repeat for all


           ; Check minimum needed kernel version 0.4.0 in order to have
           ; heap manager available.

chekvers:  ldi     high k_ver         ; pointer to installed kernel version
           phi     rd
           ldi     low k_ver
           plo     rd

           lda     rd                 ; if major is non-zero then good
           lbnz    allocmem

           lda     rd                 ; if minor is 4 or more then good
           smi     4
           lbdf    allocmem

           sep     scall              ; quit with error message
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
           sep     sret


           ; Allocate a page-aligned block from the heap for storage of
           ; the persistent code module. Make it permanent so it will
           ; not get cleaned up at program exit.

allocmem:  ldi     high modend-module  ; length of persistent module
           phi     rc
           ldi     low modend-module
           plo     rc

           ldi     255                 ; page-aligned
           phi     r7
           ldi     4                   ; permanent
           plo     r7

           sep     scall               ; request memory block
           dw      o_alloc
           lbnf    gotalloc

           sep     scall               ; return with error
           dw      o_inmsg
           db      'ERROR: Could not allocate memeory from heap',13,10,0
           sep     sret

gotalloc:  ghi     rf                  ; Offset to adjust addresses with
           smi     high module
           stxd


           ; Copy module code into the permanent heap block

           ldi     high modend-module  ; length of code to copy
           phi     rc
           ldi     low modend-module
           plo     rc

           ldi     high module         ; get source address
           phi     rd
           ldi     low module
           plo     rd

copycode:  lda     rd                  ; copy code to destination address
           str     rf
           inc     rf
           dec     rc
           glo     rc
           lbnz    copycode
           ghi     rc
           lbnz    copycode


           ; Output banner message

           sep     scall
           dw      o_inmsg
           db      'Hydro IDE Driver Build 0 for Elf/OS',13,10,0


           ; Test if DMA is supported by trying a DMA operation and seeing
           ; if R0 changes. Also test for high DMAIN latency that causes
           ; an extra DMA cycle by seeing if the transfer is not 512 bytes.
           ; On hardware with slow DMAIN an extra byte will be transferred.
           ; For simplicity, we don't actually read disk contents, rather we
           ; just DMAIN the sector count buffer of the drive over and over.

           ldi     0                  ; run test 256 times to make sure that
           plo     rc                 ;  dmain isn't on the edge of being ok

dmatest:   ldi     high buffer        ; setup dma buffer pointer
           phi     r0
           ldi     low buffer
           plo     r0

           sex     r3                 ; use inline arguments

           out     cf_addr            ; set one sector to transfer
           db      cf_count + 1

           out     cf_addr            ; enable dma in of count register
           db      cf_dmain + ide_coun

           sex     r2                 ; switch back to r2
 
           glo     r0                 ; if r0.0 is not equal to the starting
           smi     low buffer         ;  value still, we did the wrong count,
           lbnz    fixupdma           ;  we want to use the fixup routine

           dec     rc                 ; loop the full 255 tests
           glo     rc
           bnz     dmatest

           ghi     r0                 ; if r0.0 was right on all, then check
           smi     high buffer        ;  if r0.1 even changed, if not then
           lbnz    intisdma           ;  this hardware doesn't support dma


           ; Patch BIOS to point to PIO disk routines

           ldi     high patchpio      ; Get point to table of patch points
           phi     rd
           ldi     low patchpio
           plo     rd

           sep     scall
           dw      o_inmsg
           db      'Installing PIO mode',13,10,0

           lbr     setpatch


           ; Patch BIOS to point to DMA disk routines with fixup code

fixupdma:  ldi     high patchfix     ; Get point to table of patch points
           phi     rd
           ldi     low patchfix
           plo     rd

           sep     scall
           dw      o_inmsg
           db      'Installing DMA mode (with overclock fix)',13,10,0

           lbr     setpatch


           ; Patch BIOS to point to normal DMA disk routines

intisdma:  ldi     high patchdma     ; Get point to table of patch points
           phi     rd
           ldi     low patchdma
           plo     rd

           sep     scall
           dw      o_inmsg
           db      'Installing DMA mode',13,10,0


           ; Update kernel hooks to point to the copied module code

setpatch:  inc     r2                 ; point to page offset on stack

hookloop:  lda     rd                 ; a zero marks end of the table
           lbz     finished

           phi     rf                 ; get pointer to vector to hook
           lda     rd
           plo     rf

           inc     rf                 ; skip the lbr opcode

           lda     rd                 ; add offset to get copy address
           add                        ;  and update into vector
           str     rf
           inc     rf
           lda     rd
           str     rf

           lbr     hookloop           ; repeat for all


           ; All done, exit to operating system

finished:  sep     sret


           ; Table giving addresses of jump vectors we need to update, along
           ; with offset from the start of the module to repoint those to.

patchpio:  dw      d_ideread, pioread
           dw      d_idewrite, piowrite
           db      0

patchdma:  dw      d_ideread, dmaread
           dw      d_idewrite, dmawrite
           db      0

patchfix:  dw      d_ideread, fixread
           dw      d_idewrite, dmawrite
           db      0


           org     $ + 0ffh & 0ff00h

module:    ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.


; When the interface is DMA-driven, read and write are identical operations,
; differing only in which direction D<Ais enabled and what IDE command is
; sent to the drive when the transfer is setup. The hardware takes care of
; everything else. These two entry points for read and write just push those
; appropriate values to the stack and then the rest of the code is common.
;
; Input:
;   R2 = Stack pointer
;   R7.0 = Sector start register
;   R7.1 = Cylinder low register
;   R8.0 = Cylinder high register
;   R8.1 = Head and device register
;   RF = Pointer to buffer
;
; Output:
;   D = Controller status register
;   DF = Set if error occurred ##
;   R0 = Set to same as RF
;   RF = Advanced by 512 bytes ##
; 
; ## Not documented but important as some Elf/OS code assumes this behavior.


piowrite:  ldi     low dopioout        ; address of polled output routine
           br      writecmd

pioread:   ldi     low dopioinp        ; address of polled input routine
           br      readcmd

fixread:   ldi     low dofixinp        ; address of fix-up dma input routine
           br      readcmd

dmawrite:  ldi     low dodmaout        ; address of dma output routine
writecmd:  plo     re

           ldi     ide_writ            ; write sector command
           br      cfblock

dmaread:   ldi     low dodmainp        ; address of dma input routine
readcmd:   plo     re

           ldi     ide_read            ; read sector command
cfblock:   stxd

           sex     r3                  ; use inline data value
           out     cf_addr             ; output to address port
           db      ide_stat            ; select status register

           sex     r2                  ; use stack for input
           inp     cf_data             ; read status register
           xri     ide_rdy             ; invert state of RDY bit
           ani     ide_bsy + ide_rdy   ; test BSY and RDY bits only
           bnz     cfpopret            ; error if BSY set or RDY clear

           ldi     ide_cmnd            ; command register
           stxd

           ghi     r8                  ; device number
           stxd
           ldi     ide_head            ; device and head register
           stxd

           glo     r8                  ; high byte of lba
           stxd
           ldi     ide_cylh            ; high cylinder byte register
           stxd

           ghi     r7                  ; middle byte of lba
           stxd
           ldi     ide_cyll            ; low cylinder byte register
           stxd

           glo     r7                  ; low byte of lba
           stxd
           ldi     ide_sect            ; sector number register
           stxd

           ldi     1                   ; transfer one sector
           stxd
           ldi     ide_coun            ; sector count register
           str     r2

           out     cf_addr
           out     cf_data

           out     cf_addr
           out     cf_data

           out     cf_addr
           out     cf_data

           out     cf_addr
           out     cf_data

           out     cf_addr
           out     cf_data

           out     cf_addr
           out     cf_data

           dec     r2

cfready:   inp     cf_data             ; read status register
           ani     ide_bsy             ; check if BSY set
           bnz     cfready             ; wait until its not

           ldn     r2                  ; get status value
           ani     ide_drq + ide_err   ; check if ERR or DRQ set
           bz      cfready             ; wait until either is

           ani     ide_err             ; check if ERR is set
           bnz     cfreturn            ; jump if ERR is set

           sex     r3                  ; out from inline values
           glo     re                  ; get address of i/o routine
           plo     r3                  ; jump to routine address

           ; Polled input routines for non-DMA operation unrolls the input
           ; loop by a factor of 8 to reduce loop overhead and increase speed
           ; by 2x for reads and 3x for writes over standard BIOS driver.

dopioinp:  out     cf_addr             ; select data register
           db      ide_data

           ldi     512/8               ; count for number of loops
           plo     re                  ; loop in unrolled by factor of 8
           sex     rf                  ; input to buffer

inploop:   inp     cf_data             ; input bytes from data port into
           inc     rf                  ; buffer and increment pointer
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf
           inp     cf_data
           inc     rf

           dec     re                  ; loop until all transferred
           glo     re
           bnz     inploop

           sex     r3                  ; reset to inline data values
           br      cfresume            ; check for error

dopioout:  out     cf_addr             ; select data register
           db      ide_data

           ldi     512/8               ; count for number of loops
           plo     re                  ; loop in unrolled by factor of 8
           sex     rf                  ; input to buffer

outloop:   out     cf_data             ; output bytes to data port
           out     cf_data             ; pointer will auto increment
           out     cf_data
           out     cf_data
           out     cf_data
           out     cf_data
           out     cf_data
           out     cf_data

           dec     re                  ; loop until all transferred
           glo     re
           bnz     outloop

           sex     r3                  ; reset to inline data values
           br      cfresume            ; check for error

           ; DMA-based input driver works on 1802/Mini adapter and run at
           ; approximately ten times the transfer rate of the standard BIOS
           ; driver. Needs to use R0 as DMA memory pointer.

dodmaout:  glo     rf                  ; setup dma pointer to point
           plo     r0                  ; to specified data buffer
           ghi     rf
           phi     r0

           adi     2                   ; update rf to account for transfer
           phi     rf

           out     cf_addr             ; set sector counter for dma
           db      cf_count + 1        ; to one sector

           out     cf_addr             ; enable dma for output
           db      cf_dmout            ; transfer will magically happen here

           br      cfresume            ; check for errors

dodmainp:  glo     rf                  ; setup dma pointer to point
           plo     r0                  ; to specified data buffer
           ghi     rf
           phi     r0

           adi     2                   ; update rf to account for transfer
           phi     rf

           out     cf_addr             ; set sector counter for dma
           db      cf_count + 1        ; to one sector

           out     cf_addr             ; enable dma for input
           db      cf_dmain            ; transfer will magically happen here

           br      cfresume            ; check for errors

           ; DMA input routine with fixup to save byte just past data buffer
           ; and then restore it after since on hardware with slow DMAIN
           ; latency an extra cycle will be run, transferring a garbage byte.
           ; On output the drive doesn't seem to mind the extra cycle.

dofixinp:  glo     rf                  ; setup dma pointer to point
           plo     r0                  ; to specified data buffer
           ghi     rf
           phi     r0

           adi     2                   ; update rf to account for transfer
           phi     rf
           ldn     rf                  ; save memory byte just past buffer

           out     cf_addr             ; set sector counter for dma
           db      cf_count + 1        ; to one sector

           out     cf_addr             ; enable dma for input
           db      cf_dmain            ; transfer will magically happen here

           str     rf                  ; restore saved byte, done twice just
           str     rf                  ;  as a no-op for timing reasons

           ; End of data transfer, check for errors and report as needed.

cfresume:  out     cf_addr             ; clear address register to disable
           db      ide_stat            ; dma and select status register

           sex     r2                  ; input to stack

cfcheck:   inp     cf_data             ; read status register
           xri     ide_rdy             ; check if RDY is set
           ani     ide_bsy + ide_rdy   ; and BSY is clear
           bnz     cfcheck             ; wait until they are

cfreturn:  adi     255                 ; if d=0 set df=0, if d>0 set df=1
           ldn     r2                  ; get saved status register value
           sep     r5                  ; return to caller

cfpopret:  inc     r2
           br      cfreturn

modend:    ; This is the last of what's copied to the heap.

buffer:    ds      512

end:       ; That's all folks!

