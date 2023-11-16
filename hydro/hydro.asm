
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

d_idereset: equ   0444h
d_ideread:  equ   0447h
d_idewrite: equ   044ah


          ; Hardware port definitions

#define EXP_PORT     5                  ; group i/o expander port
#define IDE_SELECT   2                  ; ide interface address port
#define IDE_DATA     3                  ; ide interface data port
#define NO_GROUP     0                  ; default if no group selected


            ; Bits in CF interface address port

#define IDE_A_COUNT 80h                 ; dma sector count
#define IDE_A_DMOUT 40h                 ; dma out enable
#define IDE_A_DMAIN 20h                 ; dma in enable


            ; IDE register addresses

#define IDE_R_DATA   00h
#define IDE_R_ERROR  01h
#define IDE_R_FEAT   01h
#define IDE_R_COUNT  02h
#define IDE_R_SECT   03h
#define IDE_R_CYLLO  04h
#define IDE_R_CYLHI  05h
#define IDE_R_HEAD   06h
#define IDE_R_STAT   07h
#define IDE_R_CMND   07h


            ; Bits in IDE status register

#define IDE_S_BUSY   80h                ; busy
#define IDE_S_RDY    40h                ; ready
#define IDE_S_DSC    10h                ; seek complete
#define IDE_S_DRQ    08h                ; data request
#define IDE_S_IDX    02h                ; index mark
#define IDE_S_ERR    01h                ; error


            ; IDE head register bits

#define IDE_H_DR0    000h               ; select drive 0
#define IDE_H_DR1    010h               ; select drive 1
#define IDE_H_LBA    0e0h               ; enable lba mode


            ; IDE command code values

#define IDE_C_READ  20h                 ; read sector
#define IDE_C_WRITE 30h                 ; write sector
#define IDE_C_IDENT 0ech                ; identify drive
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

            db    11+80h                ; month
            db    2                     ; day
            dw    2023                  ; year
            dw    10                    ; build

            db    'See github.com/dmadole/Elfos-hydro for more info',0


          ; Check if hook points have already been patched and do not
          ; install if so, since we don't know what it is or what the
          ; impact might be of disconnecting it.

entry:      sep   scall
            dw    o_inmsg
            db    'Hydro IDE Disk Driver Build 10 for Elf/OS',13,10,0


          ; Check minimum needed kernel version 0.4.0 in order to have
          ; heap manager available.

chekvers:   ldi   high k_ver            ; pointer to installed kernel version
            phi   rd
            ldi   low k_ver
            plo   rd

            lda   rd                    ; if major is non-zero then good
            lbnz  getopts

            lda   rd                    ; if minor is 4 or more then good
            smi   4
            lbdf  getopts

            sep   scall                 ; quit with error message
            dw    o_inmsg
            db    'ERROR: Needs kernel version 0.4.0 or higher',13,10,0
            sep   sret


          ; Parse the command-line arguments to build a list of port select
          ; groups to search on for drives.
          ;
          ; The -g argument builds a list of groups with no defaults, while
          ; the -s argument adds to the default configuration of a single
          ; controller on the default group. Either may be specified multiple
          ; times to build a list, or multiple arguments may be given, 
          ; separated by commas. The -g and -s options are equivalent after
          ; the first usage of either.

getopts:    ldi   groups.1              ; pointer to table of groups
            phi   r8
            ldi   groups.0
            plo   r8

            ghi   ra                    ; move arguments pointer to rf
            phi   rf
            glo   ra
            plo   rf

skipsp1:    lda   rf                    ; skip any leading spaces
            lbz   testdrv
            sdi   ' '
            lbdf  skipsp1

            smi   ' '-'-'               ; error if not a dash
            lbnz  dousage

            lda   rf                    ; if a g process list
            smi   'g'
            lbz   skipsp2

            smi   's'-'g'               ; if not an s then error
            lbnz  dousage

            glo   r8                    ; treat like g if not first
            smi   groups.0
            lbnz  skipsp2

            inc   r8                    ; leave default in list

skipsp2:    lda   rf                    ; skip spaces but not required
            lbz   dousage
            sdi   ' '
            lbdf  skipsp2

            dec   rf                    ; back up to last non-space

dogroup:    sep   scall                 ; get number
            dw    f_hexin
            lbdf  dousage

            ghi   rd                    ; error if greater than 255
            lbnz  dousage

            glo   rd                    ; store group to table
            str   r8
            inc   r8

            ldi   -1                    ; set terminator in table
            str   r8

            lda   rf                    ; if end of line then done
            lbz   testdrv
          
            sdi   ' '                   ; if space get next argument
            lbdf  skipsp1

            smi   ' '-','               ; else if comma get next group
            lbz   dogroup


          ; Display a minimally-helpful message is something went wrong.

dousage:    sep   scall                 ; error if bad syntax
            dw    o_inmsg
            db    'USAGE: hydro [-g|-s group[,group...]] ...',13,10,0

            sep   sret


          ; Check to see if a drive is present. We will do this thoroughly
          ; and remember the result so that the actual driver can take some
          ; shortcuts. This is fine since drive hot-swap is not supported.

testdrv:    ldi   groups.1              ; get pointer to groups table
            phi   r8
            ldi   groups.0
            plo   r8

            ldi   drivtbl.1
            phi   r7
            ldi   drivtbl.0
            plo   r7

            ldi   0                     ; initialize drive number
            phi   r9

testpri:    ldi   0                     ; initialize master/slave flag
            plo   r9

testsec:    phi   rf                    ; set message pointer to null

            ldn   r8                    ; if port group zero then don't set
            lbz   nogroup

            dec   r2                    ; set port expander group
            str   r2
            out   EXP_PORT
 

nogroup:    ldi   -1                    ; timeout of 2 seconds
            phi   rd
            plo   rd

            sep   scall                 ; wait until controller is ready
            dw    waitbsy
            lbdf  nodrive


          ; Now that we have confirmed busy is clear, it's ok to access
          ; other registers, so we select the drive and check for ready.

            sex   r3                    ; select lba mode and drive
            out   IDE_SELECT
            db    IDE_R_HEAD

            glo   r9                    ; get master or slave drive

            lsnz                        ; if zero select master drive
            out   IDE_DATA
            db    IDE_H_LBA+IDE_H_DR0

            lsz                         ; oetherwise select slave drive
            out   IDE_DATA
            db    IDE_H_LBA+IDE_H_DR1

            sep   scall                 ; wait until drive ready
            dw    waitrdy
            lbdf  nodrive

            ani   IDE_S_DRQ+IDE_S_IDX   ; these should not be set
            lbnz  nodrive


          ; Now that we think there is a working drive here, set it to
          ; 8-bit mode, which is necessary as the hardware does not support
          ; the high 8 data bits of the bus.

            sex   r3                    ; enable feature 8 bit mode
            out   IDE_SELECT
            db    IDE_R_FEAT
            out   IDE_DATA
            db    IDE_F_8BIT

            out   IDE_SELECT            ; send set feature command
            db    IDE_R_CMND
            out   IDE_DATA
            db    IDE_C_FEAT

            sep   scall                 ; wait until drive ready
            dw    waitrdy
            lbdf  nodrive

            ani   IDE_S_DRQ+IDE_S_ERR   ; these should not be set
            lbnz  nodrive


          ; Send an identify command as a further test that the drive works,
          ; to test whether DMA is supported, and to get some information
          ; about the drive to display, including size and model.

            sex   r3                    ; send identify command
            out   IDE_SELECT
            db    IDE_R_CMND
            out   IDE_DATA
            db    IDE_C_IDENT

            sep   scall                 ; wait until drive ready
            dw    waitrdy
            lbdf  nodrive

            ani   IDE_S_DRQ+IDE_S_ERR   ; should have drq but not err
            xri   IDE_S_DRQ
            lbnz  nodrive


          ; Test if DMA works by trying a DMAIN and seeing if R0 changes.

            ldi   sectdma.1
            phi   ra
            ldi   sectdma.0
            plo   ra

            ldi   buffer.1              ; setup dma buffer pointer
            phi   r0
            ldi   buffer.0
            plo   r0

            sex   r3                    ; set one sector to transfer
            out   IDE_SELECT
            db    IDE_A_COUNT+1

            out   IDE_SELECT            ; enable dma in of data
            db    IDE_A_DMAIN+IDE_R_DATA

            sex   r2                    ; delay while dma completes
            sex   r2

            ghi   r0                    ; dma worked if r0 advanced
            smi   (buffer+512).1
            lbz   withdma


          ; If DMA did not work then copy the identity data using PIO.

            ldi   fastpio.1
            phi   ra
            ldi   fastpio.0
            plo   ra

            ldi   0                     ; 512 loops unrolled by 2 is 256
            plo   rc

            sex   r0                    ; r0 is already set so use it

piocopy:    inp   IDE_DATA              ; input two bytes per loop
            inc   r0
            inp   IDE_DATA
            inc   r0

            dec   rc                    ; copy until all complete
            glo   rc
            lbnz  piocopy


          ; Patch BIOS to point to normal DMA disk routines

withdma:    sep   scall                 ; wait for busy clear and rdy set
            dw    waitrdy
            lbdf  nodrive

            ani   IDE_S_DRQ+IDE_S_ERR   ; if set then something is wrong
            lbnz  nodrive


          ; Do a cursory check on the identity data so that we can have some
          ; confidence that data is being read correctly. First check that
          ; the current capacity and maximum LBA address are the same.

display:    ldi   (buffer+114).1        ; pointer to current capacity
            phi   rb
            ldi   (buffer+114).0
            plo   rb

            ldi   (buffer+120).1        ; pointer to maximum lba
            phi   rd
            ldi   (buffer+120).0
            plo   rd

            ldi   4                     ; there are four bytes
            plo   rc

            sex   rb
samecap:    lda   rd                    ; test if not the same
            xor
            inc   rb
            lbnz  nodrive

            dec   rc                    ; check that all are the same
            glo   rc
            lbnz  samecap


          ; Then also check that the number of heads is between 1 and 16.

            ldi   (buffer+6).1          ; address of number of heads
            phi   rb
            ldi   (buffer+6).0
            plo   rb

            lda   rb                    ; lsb should be 1-16
            lbz   nodrive
            sdi   16
            lbnf  nodrive

            lda   rb                    ; msb should always be zero
            lbnz  nodrive


          ; Now that we have found a drive and gotten identity data, format
          ; and output some information about it.

            ldi   string.1              ; pointer to string buffer
            phi   rf
            ldi   string.0
            plo   rf


          ; First output the poisition number of the drive.

cpystr1:    lda   ra                    ; copy 'disk ' until terminator
            str   rf
            inc   rf
            lbnz  cpystr1

            dec   rf                    ; back to last character

            phi   rd                    ; get drive number
            ghi   r9
            plo   rd

            adi   1
            phi   r9

            sep   scall                 ; output drive number to string
            dw    f_intout


cpystr2:    lda   ra                    ; copy ': ' until terminator
            str   rf
            inc   rf
            lbnz  cpystr2

            dec   rf                    ; back to last character


          ; Next output the size. Do so in gigabytes for larger disks or
          ; in megabytes for smaller disks.

            ldi   (buffer+123).1        ; get pointer to sector count msb
            phi   rb
            ldi   (buffer+123).0
            plo   rb

            ldn   rb                    ; check if display in gigs
            smi   8
            lbnf  notgigs

            ldi   0                     ; highest byte always zero
            plo   rc

            ldn   rb                    ; get middle byte
            phi   rd

            dec   rb                    ; get low byte, set fifth bit from
            ldn   rb                    ;  right to shift five times
            ani   0e0h
            ori   010h
            plo   rd

            ldi   'G'                   ; size suffix
            phi   rc

            lbr   doshift               ; divide by 32 to get gigs

notgigs:    ldn   rb                    ; get high byte
            plo   rc

            dec   rb                    ; get middle byte
            ldn   rb
            phi   rd

            dec   rb                    ; get low byte, set third bit from
            ldn   rb                    ;  right to shift three times
            ani   0f8h
            ori   004h
            plo   rd

            ldi   'M'                   ; size suffix
            phi   rc

doshift:    glo   rc                    ; shift to divide size
            shr
            plo   rc
            ghi   rd
            shrc
            phi   rd
            glo   rd
            shrc
            plo   rd

            lbnf  doshift               ; repeat until set bit comes out

            sep   scall                 ; output size
            dw    f_intout

            ldi   ' '                   ; output space
            str   rf
            inc   rf

            ghi   rc                    ; output g or m prefix
            str   rf
            inc   rf


          ; Output the transfer type of the controller from the static
          ; string in the previously chosen template.

cpystr3:    lda   ra                    ; copy type until terminator
            str   rf
            inc   rf
            lbnz  cpystr3

            dec   rf


          ; Output the model number of the drive. Because of the ordering in
          ; the table and the way 8-bit mode works, the even and odd pairs
          ; of bytes are swapped inthe buffer, so we need to undo that.

            ldi   (buffer+54).1         ; get pointer to model number
            phi   rd
            ldi   (buffer+54).0
            plo   rd

            ldi   40/2                  ; count of number of swaps
            plo   rc

cpmodel:    lda   rd                    ; swap even and odd bytes
            plo   re
            lda   rd
            str   rf
            inc   rf
            glo   re
            str   rf
            inc   rf

            dec   rc                    ; repeat for all of model name
            lbnz  cpmodel

truncat:    dec   rf                    ; find last non-space character
            ldn   rf
            sdi   ' '
            lbdf  truncat

            inc   rf                    ; back to first space


          ; Add the terminators to the string and get a pointer back to the
          ; beginning to display it from.

cpystr4:    lda   ra                    ; copy cr, lf and terminator
            str   rf
            inc   rf
            lbnz  cpystr4

            ldi   string.1              ; pointer back to start of string
            phi   rf
            ldi   string.0
            plo   rf


          ; Since we found a drive, store the head selector, which contains
          ; the drive bit, as well as the port group selector. The driver
          ; later references this table to map drive numbers to hardware.

            ldn   r8                    ; copy group into table
            str   r7
            inc   r7

            glo   r9                    ; copy head register to table

            lsz                         ; drive 1 selector if non-zero
            ldi   IDE_H_LBA+IDE_H_DR1

            lsnz
            ldi   IDE_H_LBA+IDE_H_DR0   ; drive 0 selector if zero

            str   r7                    ; store into table
            inc   r7


          ; We now have either found a drive and it's description in pointed
          ; to by RF or we didn't find one and the high byte of RF is null.

nodrive:    ldn   r8                    ; if default group then don't reset
            lbz   dispdrv

            sex   r3                    ; else reset back to default group
            out   EXP_PORT
            db    NO_GROUP
            sex   r2

dispdrv:    ghi   rf                    ; null if no drive was found
            lbz   notdisp

            sep   scall                 ; else display drive information
            dw    o_msg


          ; Advance our counter and pointer and go back and check for the
          ; next drive if not at the end of controller list.

notdisp:    ghi   r9                    ; stop looking once four drives
            smi   4
            lbz   loadmod

            glo   r9                    ; if drive zero, make one and test
            inc   r9
            lbz   testsec

            inc   r8                    ; check next group if not last one
            ldn   r8
            smi   -1
            lbnz  testpri

            ghi   r9                    ; if any drives found, load module
            lbnz  loadmod

            sep   scall                 ; else display error
            dw    o_inmsg
            db    'ERROR: No drives found so module not loaded',13,10,0

            sep   sret


          ; Allocate a page-aligned block from the heap for storage of
          ; the persistent code module. Make it permanent so it will
          ; not get cleaned up at program exit.

loadmod:    phi   rc                    ; length of module in rc
            ldi   modend.0
            plo   rc

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
            smi   module.1
            str   r2


          ; Copy common module code into the permanent heap block

            ldi   module.1              ; get source address
            phi   rb
            glo   rf
            plo   rb

copymod:    lda   rb                    ; copy code to destination address
            str   rf
            inc   rf
            dec   rc
            glo   rc
            lbnz  copymod


          ; Update kernel hooks to point to the copied module code

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

finished:   sep   sret

sectdma:    db    'Disk ',0,': ',0,'B, Sector DMA, ',0,13,10,0 
fastpio:    db    'Disk ',0,': ',0,'B, Fast PIO, ',0,13,10,0




waitrdy:    sex   r3                    ; select status register
            out   IDE_SELECT
            db    IDE_R_STAT
            sex   r2

            smi   0                     ; if status is 00 fail immediately
            inp   IDE_DATA
            lbz   exitrdy

looprdy:    inp   IDE_DATA              ; wait for busy clear and ready set
            smi   IDE_S_RDY
            smi   IDE_S_RDY
            lbnf  exitrdy

            dec   rd                    ; use remainder of busy timeout
            ghi   rd
            lbnz  looprdy

exitrdy:    ldx
            sep   sret


waitbsy:    sex   r3                    ; select status register
            out   IDE_SELECT
            db    IDE_R_STAT

loopbsy:    sex   r2
            inp   IDE_DATA              ; wait for busy clear and ready set
            smi   IDE_S_BUSY
            lbnf  exitbsy

            dec   rd                    ; use remainder of busy timeout
            ghi   rd
            lbnz  loopbsy

exitbsy:    ldx
            sep   sret


          ; Table giving addresses of jump vectors we need to update, along
          ; with offset from the start of the module to repoint those to.

patchtbl:   dw    d_idereset, idereset
            dw    d_ideread, ideread
            dw    d_idewrite, idewrite
            db    0


          ; Start the actual module code on a new page so that it forms
          ; a block of page-relocatable code that will be copied to himem.

            org   $ + 0ffh & 0ff00h

module:   ; The bus reset function is implemented as a no-op since everything
          ; is initialized when the driver is loaded and there is no need to
          ; repeat this operation later since hot-swap is not supported.
          ;
          ; Takes no arguments and returns no results except clearing DF.

idereset:   adi   0
            sep   sret


          ; The IDEREAD and IDEWRIT functions have the same API, differing
          ; only in which direction the data is transferred.
          ;
          ; Input:
          ;   R2 = Stack pointer
          ;   R7 = Block address bits 0-15
          ;   R8.0 = Block address bits 16-23
          ;   R8.1 = Drive number in bits 0-4
          ;   RF = Pointer to buffer
          ;
          ; Output:
          ;   D = Modified
          ;   DF = Set if error, else cleared
          ;   R0 = Modified
          ;   RF = Advanced by 512 bytes
          ;
          ; Notes:
          ;   DF,RF: Not documented but important as some code assumes this.
          ;   R0: From use as DMA pointer. Not standard for all disk drivers.
          ;   D: Pico/Elf BIOS returns IDE status register but nothing cares.

ideread:    glo   r3                    ; subroutine to setup command
            br    precmnd

            out   IDE_DATA              ; send read sector command
            db    IDE_C_READ

            glo   r3                    ; subroutine to setup data transfer
            br    prexfer

          ; On systems that are overclocked or have slow busses, the end of
          ; the DMA operation may lag by a cycle, causing 513 bytes to be
          ; transferred instead of 512. This causes the byte following the
          ; buffer to be overwritten, so we save that byte first, then check
          ; for that condition and restore the byte if needed.

            ldn   rf                    ; get byte just past buffer

            out   IDE_SELECT            ; start dma input operation
            db    IDE_A_DMAIN+IDE_R_DATA

          ; The following instruction is here instead of above because an
          ; extra instruction delay may be needed before DMA actually starts
          ; due to where in the machine cycle DMAIN is sampled.

            plo   re                    ; save byte just past buffer
            sex   r2
            ghi   r0                    ; if dma did not happen then do pio
            sm
            bnz   pioread

            glo   rf                    ; if end address is right then done
            str   r2
            glo   r0
            sm
            bz    restret

            glo   re                    ; else fix overrun byte then done
            str   rf
            br    restret


           ; Input routine for non-DMA read operation unrolls the input loop
           ; by a factor of 8 to reduce loop overhead and increase speed by
           ; by over 2x the Pico/Elf BIOS driver speed.

pioread:    ldi   512/8               ; count for number of loops
            plo   re                  ; loop in unrolled by factor of 8

            sex   r0
rdloop:     inp   IDE_DATA            ; input bytes from data port into
            inc   r0                  ; buffer and increment pointer
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0
            inp   IDE_DATA
            inc   r0

            dec   re                    ; loop until all transferred
            glo   re
            bnz   rdloop

            br    restret               ; done so restore and return


          ; A write operation is mostly just like a read except for the
          ; direction of data transfer. For a write though, a DMA overrun
          ; is harmless, the drive just ignores the extra byte, so no special
          ; handling is needed like it is for read.

idewrite:   glo   r3                    ; subroutine to setup command
            br    precmnd

            out   IDE_DATA              ; write sector command
            db    IDE_C_WRITE

            glo   r3                    ; subroutine to setup data transfer
            br    prexfer

            out   IDE_SELECT            ; trigger dma out operation
            db    IDE_A_DMOUT+IDE_R_DATA

            sex   r2                    ; additional delay needed for dma
            sex   r2

            ghi   r0                    ; if data transferred then done
            sm
            bz    restret

          ; If the DMA did not happen then we will do the data transfer by
          ; regular port I/O. The loop is unrolled by a factor of eight to
          ; speed things up a bit, about three times Pico/Elf BIOS speed.

            ldi   512/8                 ; loop is unrolled by factor of 8

            sex   r0
wrtloop:    out   IDE_DATA              ; output bytes to data port
            out   IDE_DATA              ; pointer will auto increment
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA
            out   IDE_DATA

            smi   1                     ; loop until all transferred
            bnz   wrtloop


          ; After the completion of data transfer, we need to check for any
          ; error and reset the port group to the default if it was changed.

restret:    sex   r3                    ; select status register
            out   IDE_SELECT
            db    IDE_R_STAT

            sex   r2
            inp   IDE_DATA              ; move err flag into df to return
iserror:    shr

            inc   r2                    ; reset group if one was set
            ldn   r2
            bz    return

            sex   r3                    ; put back to default group
            out   EXP_PORT
            db    NO_GROUP

return:     sep   sret                  ; return to caller


          ; Most of the controller interaction is the same whether its for
          ; a read or write so this subroutine is common setup for sending
          ; a block I/O command.

precmnd:    adi   2                     ; save return address
            plo   re

            ghi   r8                    ; error if drive 4 or higher
            ani   31
            smi   4
            bdf   return

          ; We use R0 as a scratch register without saving it since we are
          ; going to change it later anyway and it's a deiscated DMA pointer.
          ; But note that LDN R0 is not a legal instruction so LDA instead.

            shl                         ; get pointer into disk table
            adi   9+drivtbl.0           ; note that this sets df
            plo   r0
            ghi   r3
            phi   r0

            lda   r0                    ; error if head register is zero
            bz    return

            dec   r0                    ; get group port number
            dec   r0
            lda   r0

            stxd                        ; save for later and for now
            str   r2

            lsz                         ; select group if not zero
            out   EXP_PORT
            dec   r2

            sex   r3                    ; select status register
            out   IDE_SELECT
            db    IDE_R_STAT

            sex   r2
loopdr1:    inp   IDE_DATA              ; wait until drive not busy
            ani   IDE_S_BUSY
            bnz   loopdr1

            sex   r3
            out   IDE_SELECT            ; select head register
            db    IDE_R_HEAD
            sex   r0
            out   IDE_DATA

            sex   r3
            out   IDE_SELECT            ; select status register
            db    IDE_R_STAT

            sex   r2
loopdr2:    inp   IDE_DATA              ; wait until drive ready
            ani   IDE_S_RDY
            bz    loopdr2

            glo   r8                    ; push lba onto the stack
            stxd
            ghi   r7
            stxd
            glo   r7
            str   r2

            sex   r3
            out   IDE_SELECT            ; set lba low byte (r7.0)
            db    IDE_R_SECT
            sex   r2
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
            dec   r2

            sex   r3                    ; set sector count to one
            out   IDE_SELECT
            db    IDE_R_COUNT
            out   IDE_DATA
            db    1

            out   IDE_SELECT            ; execute read or write command
            db    IDE_R_CMND

            glo   re                    ; return to ideread or idewrite
            plo   r3


          ; Similar to the prior, much of the preparation for data transfer
          ; is the same for both read and write to it's collected in this
          ; subroutine used for both.

prexfer:    adi   2                     ; save return address
            plo   re

            sex   r2
loopdr3:    inp   IDE_DATA              ; wait until drive not busy
            ani   IDE_S_BUSY
            bnz   loopdr3

            ldn   r2                    ; if an error, skip transfers
            ani   IDE_S_ERR
            bnz   iserror

            glo   rf                    ; set dma pointer to buffer
            plo   r0
            ghi   rf
            phi   r0

            adi   2                     ; advance and save for dma check
            phi   rf
            str   r2

            sex   r3                    ; set dma count to one sector
            out   IDE_SELECT
            db    IDE_A_COUNT+1

            glo   re                    ; return to ideread or idewrite
            plo   r3


          ; This is a table of the drives that were found at load time. For
          ; each, we store the port group selector, or zero if none, and
          ; the head register value, which selects master or slave.

drivtbl:    db    0,0
            db    0,0
            db    0,0
            db    0,0

          ; The minfo command will look at the top of a heap block for a
          ; name if the 64 flag is set on the block, so here is a name. It
          ; needs some zero padding to copy into the block as well in case
          ; o_alloc returns a larger block than requested (hy up to 3 bytes)
          ; which can happen. Minfo knows to ignore this extra padding.

            db    0,'Hydro',0           ; label for minfo heap block, with
modend:     db    0,0,0                 ;  padding if alloc returns excess

          ; This is a table that is built from the command line options of
          ; which port groups to probe for disks, terminated with -1. Note
          ; that only the first two bytes are stored in the executable.

groups:     db    0,-1                  ; default to just group 0
            ds    20

          ; Buffer for reading the disk identity information during probing.
          ; This is accounted for in the size in the header but not actually
          ; written into the executable file, as with anything else following.

buffer:     ds    512

          ; Buffer for composing a disk information output line into.

string:     ds    80

end:      ; That's all folks!

