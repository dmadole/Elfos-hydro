
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

            #include include/bios.inc
            #include include/kernel.inc


          ; Define non-published API elements

d_ideread:  equ   0447h
d_idewrite: equ   044ah


          ; Hardware port definitions

#define EXP_PORT     5                  ; group i/o expander port
#define IDE_GROUP    0                  ; ide interface group
#define IDE_SELECT   2                  ; ide interface address port
#define IDE_DATA     3                  ; ide interface data port


            ; Bits in CF interface address port

#define IDE_A_COUNT 80h                 ; dma sector count
#define IDE_A_DMOUT 40h                 ; dma out enable
#define IDE_A_DMAIN 20h                 ; dma in enable
#define IDE_A_STOP  00h                 ; dma in enable


            ; IDE register addresses

#define IDE_R_DATA  00h
#define IDE_R_ERROR 01h
#define IDE_R_FEAT  01h
#define IDE_R_COUNT 02h
#define IDE_R_SECT  03h
#define IDE_R_CYLLO 04h
#define IDE_R_CYLHI 05h
#define IDE_R_HEAD  06h
#define IDE_R_STAT  07h
#define IDE_R_CMND  07h


            ; Bits in IDE status register

#define IDE_S_BUSY  80h                 ; busy
#define IDE_S_RDY   40h                 ; ready
#define IDE_S_DRQ   08h                 ; data request
#define IDE_S_ERR   01h                 ; error


            ; IDE head register bits

#define IDE_H_DR0   000h
#define IDE_H_DR1   010h
#define IDE_H_CHS   0a0h
#define IDE_H_LBA   0e0h


            ; IDE command code values

#define IDE_C_READ  20h                 ; read sector
#define IDE_C_WRITE 30h                 ; write sector
#define IDE_C_FEAT  0efh                ; set feature


            ; IDE features

#define IDE_F_8BIT  01h                 ; 8-bit mode


          ; Executable program header

            org   2000h - 6
            dw    start
            dw    end-start
            dw    start

start:      br    entry


          ; Build information

            db    1+80h                 ; month
            db    23                    ; day
            dw    2023                  ; year
            dw    9                     ; build

            db    'See github.com/dmadole/Elfos-hydro for more info',0


          ; Check if hook points have already been patched and do not
          ; install if so, since we don't know what it is or what the
          ; impact might be of disconnecting it.

entry:      ldi   high patchtbl         ; Get point to table of patch points
            phi   rd
            ldi   low patchtbl
            plo   rd

chekloop:   lda   rd                    ; a zero marks end of the table
            lbz   chekvers

            phi   rf                    ; get pointer to patch point
            lda   rd
            plo   rf

            inc   rf                    ; skip the lbr opcode

            ldn   rd                    ; if points into bios then ok
            smi   0f8h
            lbnf  cheknext

            sep   scall                 ; quit with error message
            dw    o_inmsg
            db    'ERROR: Read or write hooks already installed',13,10,0
            sep   sret

cheknext:   inc   rd                    ; skip target address in table
            inc   rd

            lbr   chekloop              ; repeat for all


          ; Check minimum needed kernel version 0.4.0 in order to have
          ; heap manager available.

chekvers:   ldi   high k_ver            ; pointer to installed kernel version
            phi   rd
            ldi   low k_ver
            plo   rd

            lda   rd                    ; if major is non-zero then good
            lbnz  dmatest

            lda   rd                    ; if minor is 4 or more then good
            smi   4
            lbdf  dmatest

            sep   scall                 ; quit with error message
            dw    o_inmsg
            db    'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
            sep   sret


          ; Test if DMA is supported by trying a DMA operation and seeing if
          ; R0 changes. For simplicity, we don't actually read disk contents,
          ; rather we just DMAIN from the sector count register of the drive.

dmatest:    ldi   high buffer           ; setup dma buffer pointer
            phi   r0
            ldi   low buffer
            plo   r0

            sex   r3                    ; set one sector to transfer
            out   IDE_SELECT
            db    IDE_A_COUNT + 1

            out   IDE_SELECT            ; enable dma in of count register
            db    IDE_A_DMAIN + IDE_R_COUNT

            sex   r2                    ; delay while dma completes
            sex   r2

            ghi   r0                    ; check if r0.1 changed to know if
            smi   high buffer           ;  dma happened
            lbnz  intisdma




            ldi   pioend.0              ; get length of module
            plo   rc

            ldi   piostart.1            ; get page of module
            phi   rd

            ldi   piomesg.1
            phi   r9
            ldi   piomesg.0
            plo   r9

            lbr   allocmem


piomesg:    db    'Installing PIO mode',13,10,0
dmamesg:    db    'Installing DMA mode',13,10,0


          ; Patch BIOS to point to normal DMA disk routines

intisdma:   ldi   dmaend.0              ; get length of module
            plo   rc

            ldi   dmastart.1            ; get page of module
            phi   rd

            ldi   dmamesg.1
            phi   r9
            ldi   dmamesg.0
            plo   r9


          ; Allocate a page-aligned block from the heap for storage of
          ; the persistent code module. Make it permanent so it will
          ; not get cleaned up at program exit.

allocmem:   ldi   0                     ; length of module in rc
            phi   rc

            ldi   255                   ; page-aligned
            phi   r7
            ldi   4+64                  ; permanent and named
            plo   r7

            sep   scall                 ; request memory block
            dw    o_alloc
            lbnf  gotalloc

            sep   scall                 ; return with error
            dw    o_inmsg
            db    'ERROR: Could not allocate memeory from heap',13,10,0
            sep   sret

gotalloc:   ghi   rf                    ; Offset to adjust addresses with
            smi   high module
            stxd


          ; Copy common module code into the permanent heap block

            ldi   modend.0              ; length of code to copy
            plo   re

            ldi   module.1              ; get source address
            phi   rb
            ldi   module.0
            plo   rb

copymod:    lda   rb                    ; copy code to destination address
            str   rf
            inc   rf
            dec   rc
            dec   re
            glo   re
            lbnz  copymod


          ; Copy mode-specific module code into the permanent heap block

            ldi   modend.0
            plo   rd

copyext:    lda   rd                    ; copy code to destination address
            str   rf
            inc   rf
            dec   rc
            glo   rc
            lbnz  copyext


         ; Output banner message

            sep   scall
            dw    o_inmsg
            db    'Hydro IDE Driver Build 9 for Elf/OS',13,10,0

            ghi   r9
            phi   rf
            glo   r9
            plo   rf

            sep   scall
            dw    o_msg


          ; Update kernel hooks to point to the copied module code

            inc   r2                    ; point to page offset on stack

            ldi   high patchtbl         ; Get point to table of patch points
            phi   rd
            ldi   low patchtbl
            plo   rd

hookloop:   lda   rd                    ; a zero marks end of the table
            lbz   finished

            phi   rf                    ; get pointer to vector to hook
            lda   rd
            plo   rf

            inc   rf                    ; skip the lbr opcode

            lda   rd                    ; add offset to get copy address
            add                         ;  and update into vector
            str   rf
            inc   rf
            lda   rd
            str   rf

            lbr   hookloop              ; repeat for all


          ; All done, exit to operating system

finished:   ldi   0e1h
            phi   r8

            sep   scall
            dw    cfreset

            sep   sret


          ; Table giving addresses of jump vectors we need to update, along
          ; with offset from the start of the module to repoint those to.

patchtbl:   dw    d_ideread, dmaread
            dw    d_idewrite, dmawrite
            db    0


            org   $ + 0ffh & 0ff00h

module:   ; Start the actual module code on a new page so that it forms
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

cfreset:    sex   r3

          #if IDE_GROUP
            out   EXP_PORT              ; set ide expander group
            db    IDE_GROUP
          #endif

            glo   r3
            br    waitbsy

            glo   r3
            br    drivrdy
            bdf   return

            sex   r3                     ; enable feature 8 bit mode
            out   IDE_SELECT
            db    IDE_R_FEAT
            out   IDE_DATA
            db    IDE_F_8BIT

            out   IDE_SELECT            ; send set feature command
            db    IDE_R_CMND
            out   IDE_DATA
            db    IDE_C_FEAT

waitret:    glo   r3
            br    waitbsy

            glo   r3
            br    waitrdy
            bdf   return

            ldn   r2
            shr
return:
          #if IDE_GROUP
            sex   r3
            out   EXP_PORT              ; leave as default group
            db    NO_GROUP
          #endif

            sep   sret




          ; Subroutine to check if its safe to access registers by waiting
          ; for the ready bit cleared in the status register. On the Pico/Elf
          ; the value of the INP instruction that is deposited in D is not
          ; reliable, so use the data written to memory at M(RX) instead.

waitbsy:    adi   2                     ; get return address
            plo   re

            sex   r3                    ; select status register
            out   IDE_SELECT
            db    IDE_R_STAT

            sex   r2
bsyloop:    inp   IDE_DATA              ; get register, read from memory
            ldx                         ;  not from d register, important
            ani   IDE_S_BUSY
            bnz   bsyloop

            glo   re                    ; return
            plo   r3


          ; Subroutine to select the appropriate drive and then wait for the
          ; ready bit to be set by juming into WAITRDY afterwards.

drivrdy:    adi   2                     ; get return address
            plo   re

            ghi   r8                    ; error if drive number is greater
            ani   31                    ;  than one, else put it into df
            shr
            bnz   waiterr

            sex   r3
            out   IDE_SELECT            ; select head, jump based on drive
            db    IDE_R_HEAD
            bdf   drive1

            out   IDE_DATA              ; select drive zero, jump to wait
            db    IDE_H_LBA+IDE_H_DR0   ;  for ready bit
            br    statsel

drive1:     out   IDE_DATA              ; select drive one, jump to wait
            db    IDE_H_LBA+IDE_H_DR1   ;  for ready bit
            br    statsel


          ; Subroutine to wait for ready bit set on current drive. See the
          ; note under WAITBSY regarding INP of the status register.

waitrdy:    adi   2                     ; get return address
            plo   re

            sex   r3
statsel:    out   IDE_SELECT            ; select status register
            db    IDE_R_STAT

            sex   r2                    ; input to stack

rdyloop:    inp   IDE_DATA              ; if status register is zero,
            ldx                         ;  second drive is not present
            bz    waiterr

            ani   IDE_S_RDY             ; wait until rdy bit is set
            bz    rdyloop

            adi   0                     ; return success
            glo   re
            plo   r3

waiterr:    smi   0                     ; return failure
            glo   re
            plo   r3


          ; Disk read and write share mostly common code, there is just a
          ; difference in two varaibles: what command to send to the drive
          ; and what DMA direction to enable for the transfer. So we just
          ; set these onto the stack appropriately before the routine.
          ;
          ; There are two different write entry points, one for PIO for
          ; Pico/Elf or other hardware that does not support DMA, and one
          ; for 1802/Mini using DMA.




            ; Setup read or write operation on drive.

ideblock:   stxd                        ; save command value

            glo   r3                    ; wait until not busy
            br    waitbsy

            glo   r3                    ; wait until drive ready, error if
            br    drivrdy               ;  drive is not present
            bnf   isready

            inc   r2                    ; discard command and code address
            inc   r2                    ;  and return failure
            br    return

isready:    sex   r3                    ; set sector count to one
            out   IDE_SELECT
            db    IDE_R_COUNT
            out   IDE_DATA
            db    1

            out   IDE_SELECT            ; select lba low byte register
            db    IDE_R_SECT

            sex   r2                    ; push the lba high and middle
            glo   r8                    ;  bytes onto the stack
            stxd
            ghi   r7
            stxd

            glo   r7                    ; set lba low byte (r7.0)
            str   r2
            out   IDE_DATA

            sex   r3                    ; set lba middle byte (r7.1)
            out   IDE_SELECT
            db    IDE_R_CYLLO
            sex   r2
            out   IDE_DATA

            sex   r3                    ; set lha high byte (r8.0)
            out   IDE_SELECT
            db    IDE_R_CYLHI
            sex   r2
            out   IDE_DATA

            sex   r3                     ; execute read or write command
            out   IDE_SELECT
            db    IDE_R_CMND
            sex   r2
            out   IDE_DATA

            dec   r2                    ; make room for input value

drvbusy:    inp   IDE_DATA              ; wait until drive not busy, read
            ldx                         ;  from memory, not from d
            ani   IDE_S_BUSY
            bnz   drvbusy

            ldx                         ; wait until drq or err is set
            ani   IDE_S_DRQ+IDE_S_ERR
            bz    drvbusy

            inc   r2                    ; discard status register value,
            shr                         ;  return error if err bit set
            bdf   return

            ldn   r2                    ; jump to dmawrt or dmaread
            plo   r3

modend:     ; end of the base module

dmastart:   ; start of the dma module

dmaread:    ldi   dodmard.0             ; address of dma input routine
            stxd
            ldi   IDE_C_READ            ; read sector command
            br    ideblock

dmawrite:   ldi   dodmawr.0             ; address of dma output routine
            stxd
            ldi   IDE_C_WRITE           ; write sector command
            br    ideblock

dodmawr:    glo   rf                    ; set dma pointer to data buffer
            plo   r0
            ghi   rf
            phi   r0

            adi   2                     ; advance buffer pointer past end
            phi   rf

            sex   r3                    ; set dma count to one sector
            out   IDE_SELECT
            db    IDE_A_COUNT+1

            out   IDE_SELECT
            db    IDE_A_DMOUT

            br    waitret

dodmard:    glo   rf                    ; set dma pointer to data buffer
            plo   r0
            str   r2                    ; put lsb on stack for compare later
            ghi   rf
            phi   r0

            adi   2                     ; advance buffer pointer past end
            phi   rf

            sex   r3                    ; set dma count to one sector
            out   IDE_SELECT
            db    IDE_A_COUNT+1

            ldn   rf                    ; save byte just past end of buffer
            plo   re

            out   IDE_SELECT            ; start dma input operation
            db    IDE_A_DMAIN

            sex   r2                    ; extra instruction for timing
            sex   r2

            glo   r0                    ; if no dma overrun, complete
            sm
            bz    waitret

            glo   re                    ; fix overrun byte, then complete
            str   rf
            br    waitret

            db    0,'Hydro',0           ; label for minfo

dmaend:   ; Last of dma-based module code

            db    0,0,0,0               ; padding if alloc returns excess

            org   dmastart+100h

piostart:   ; start of the pio module

           ; Polled input routines for non-DMA operation unrolls the input
           ; loop by a factor of 8 to reduce loop overhead and increase speed
           ; by over 2x for reads and 3x for writes over BIOS driver.

pioread:    ldi   dopiord.0             ; address of polled input routine
            stxd
            ldi   IDE_C_READ            ; read sector command
            br    ideblock+100h

piowrite:   ldi   dopiowr.0             ; address of polled output routine
            stxd
            ldi   IDE_C_WRITE           ; write sector command
            br    ideblock+100h

dopiord:    sex   r3

            out   IDE_SELECT          ; select data register
            db    IDE_R_DATA 

            ldi   512/8               ; count for number of loops
            plo   re                  ; loop in unrolled by factor of 8

            sex   rf
rdloop:     inp   IDE_DATA            ; input bytes from data port into
            inc   rf                  ; buffer and increment pointer
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf
            inp   IDE_DATA
            inc   rf

            dec   re                    ; loop until all transferred
            glo   re
            bnz   rdloop

            br    waitret+100h          ; check for error


dopiowr:    sex   r3

            out   IDE_SELECT            ; select data register
            db    IDE_R_DATA

            ldi   512/8                 ; count for number of loops
            plo   re                    ; loop in unrolled by factor of 8

            sex   rf
wrloop:     out   IDE_DATA              ; output bytes to data port
            out   IDE_DATA              ; pointer will auto increment
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA

            dec   re                    ; loop until all transferred
            glo   re
            bnz   wrloop

            br    waitret+100h          ; check for error


            db    0,'Hydro',0           ; label for minfo

pioend:   ; Last of pio-based module code

            db    0,0,0,0               ; padding if alloc returns excess


buffer:     ds      512

end:      ; That's all folks!

