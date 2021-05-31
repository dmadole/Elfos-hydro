; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.
;
; The author grants a license to Michael H. Riley to use this
; code for any purpose he sees fit, including commercial use,
; and without any need to include the above notice.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc

           ; Define non-published API elements

version    equ     0400h
himem      equ     0442h

           ; Hardware port definitions

#define    cf_addr 2
#define    cf_data 3

           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      checkver

           ; Build information

           db      2+80h              ; month
           db      19                 ; day
           dw      2021               ; year
           dw      1                  ; build
           db      'Written by David S. Madole',0


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with offset from the start
           ; of the module in himem to repoint those to.

patchtbl:  dw      f_ideread, cfread - module
           dw      f_idewrite, cfwrite - module
           db      0


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for himem variable to allocate
           ; memory for persistent module to use.

minvers:   db      0,3,1              ; minimum kernel version needed

checkver:  ldi     minvers.1          ; pointer to version needed
           phi     r7
           ldi     minvers.0
           plo     r7

           ldi     version.1          ; pointer to running version
           phi     r8
           ldi     version.0
           plo     r8

           ldi     3                  ; check three bytes
           plo     rf

           sex     r8                 ; subtract from running version

versloop:  lda     r7                 ; compare minimum vs running versions
           sd
           irx
           lbnf    versfail           ; negative, running < minimum, so fail
           bnz     chckhook           ; positive, running > minimum, so pass

           dec     rf                 ; zero, so equal, keep checking
           glo     rf
           bnz     versloop           ; if we exit this versions are same


           ; Check if R4 SCALL is already not pointing into BIOS, fail if so

chckhook:  ghi     r4
           smi     0f8h
           lbnf    hookfail
           

           ; Check where we are installing in memory and setup the length
           ; and appropriate address of the block. If loading to high memory
           ; adjust the himem variable to memorialize the allocation.

allocmem:  ldi     (end-module).1     ; get length of code to install
           phi     rf
           ldi     (end-module).0
           plo     rf

           ldi     0                  ; target will always be page boundary
           plo     r8
           plo     r9

           ldi     himem.1            ; pointer to top of memory variable
           phi     r7
           ldi     himem.0
           plo     r7

           sex     r7                 ; subtract instructions reference highmem
           inc     r7                 ; move to lsb of address

           glo     rf                 ; subtract size to install from himem,
           sd                         ; discard result lsb since we want to
           ghi     rf                 ; round down to a page start anyway
           dec     r7
           sdb
           smi     1                  ; add extra page for $ff00 replacement
           phi     r8
           phi     r9                 ; r8 and r9 are where we will install

           dec     r8                 ; set himem to one less than install
           ghi     r8                 ; address to reserve memory block
           str     r7
           inc     r7
           glo     r8
           str     r7
           inc     r8                 ; restore to destination address


           ; Copy the code of the persistent module to the memory address
           ; that was just determined.

copycode:  ldi     module.1           ; get source address
           phi     r7
           ldi     module.0
           plo     r7

copyloop:  lda     r7                 ; copy code to destination address
           str     r8
           inc     r8
           dec     rf
           glo     rf
           bnz     copyloop
           ghi     rf
           bnz     copyloop


           ; Get the address of the next memory page above the code we
           ; just copied and copy the memory page at $ff00 that contains
           ; BIOS API jump vectors into it.

           ldi     0                  ; addresses at start of page
           plo     r7
           plo     r8
           plo     rf                 ; clear loop counter

           ldi     0ffh               ; point to BIOS
           phi     r7

           ghi     r8                 ; point to copy target
           adi     1
           phi     r8

copyff00:  lda     r7                 ; copy the page contents
           str     r8
           inc     r8
           dec     rf
           glo     rf
           bnz     copyff00

           ghi     r8                ; drop back to start of the page
           smi     1
           phi     r8


           ; Update kernel and BIOS hooks to point to our module code. At
           ; this point, R8 points to the new BIOS jump table in RAM, and
           ; R9 points to the base address of the module code in RAM.

           ldi     patchtbl.1        ; Get point to table of patch points
           phi     r7
           ldi     patchtbl.0
           plo     r7

           sex     r7                 ; add instructions will use table

ptchloop:  lda     r7                 ; a zero marks end of the table
           bz      callpage
           phi     ra
           adi     1
           bnz     isntffxx
           ghi     r8                 ; if the address is ffxx replace it
           phi     ra                 ; with equivalent in the copy in RAM
isntffxx:  lda     r7
           plo     ra

           inc     r7                 ; point to lsb of both addresses
           inc     ra                 ; ra needs to skip the lbr opcode
           inc     ra

           glo     r9                 ; add the offset in the table to the
           add                        ; base address in RAM and update the
           str     ra                 ; address at the patch point
           dec     r7
           dec     ra
           ghi     r9
           adc
           str     ra

           inc     r7                 ; point to next entry in table and
           inc     r7                 ; continue until all are done
           br      ptchloop


           ; The new call subroutine needs to know the page address of the
           ; new copy of the jump table in high memory so patch that in.

callpage:  glo     r9                  ; get address of ldi instruction
           adi     (ldipage-module).0
           plo     r7
           ghi     r9
           adci    (ldipage-module).1
           phi     r7

           inc     r7                  ; point to ldi argument and set
           ghi     r8
           str     r7

           ; Finally, modify R4 to point to the new call subroutine so that
           ; we intercept any SCRT subroutine calls going forward.

           glo     r9
           adi     (newcall-module).0
           plo     r4
           ghi     r9
           adci    (newcall-module).1
           phi     r4


           sex     r2
           ldi     success.1
           stxd
           ldi     success.0
           stxd

           lbr     output

           org     $ + 0ffh & 0ff00h

output:    ldi     message.1
           phi     rf
           ldi     message.0
           plo     rf

           sep     scall
           dw      o_msg

           inc     r2
           lda     r2
           plo     rf
           ldn     r2
           phi     rf

           sep     scall
           dw      o_msg

           sep     sret

hookfail:  sex     r2
           ldi     hookmsg.1
           stxd
           ldi     hookmsg.0
           stxd
           br      output

versfail:  sex     r2
           ldi     vermsg.1
           stxd
           ldi     vermsg.0
           stxd
           br      output

message:   db      'Hydro Compact Flash DMA Driver Build 1 for Elf/OS',13,10,0
success:   db      'Copyright 2021 by David S Madole',13,10,0
vermsg:    db      'ERROR: Needs kernel version 0.3.1 or higher',13,10,0
hookmsg:   db      'ERROR: SCALL is already diverted from BIO','S',13,10,0


           org     $ + 0ffh & 0ff00h

module:    ; Himem loadable module starts here


; Bits in CF interface address port

cf_count   equ     80h                 ; dma sector count
cf_dmout   equ     40h                 ; dma out enable
cf_dmain   equ     20h                 ; dma in enable
cf_inten   equ     10h                 ; interrupt enable

; IDE register addresses

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


cfwrite:   dec     r2                  ; make room on stack
           ldi     cf_dmout            ; select value for dma out enable
           stxd                        ; push dma enable to stack
           ldi     ide_writ            ; ide command to write block
           br      cfblock             ; go to block i/o code

cfread:    dec     r2                  ; make room on stack
           ldi     cf_dmain            ; select value for dma in enable
           stxd                        ; push dma enable to stack
           ldi     ide_read            ; ide command to read block

cfblock:   stxd                        ; push ide command to stack

           sex     r3                  ; use inline data value
           out     cf_addr             ; output to address port
           db      ide_stat            ; status register

           sex     r2                  ; use stack for input
           inp     cf_data             ; read status register
           xri     ide_rdy             ; invert state of RDY bit
           ani     ide_bsy + ide_rdy   ; test BSY and RDY bits only
           bnz     cferror1            ; error if BSY set or RDY clear

           ghi     rf                  ; setup dma pointer to point
           phi     r0                  ; to specified data buffer
           glo     rf
           plo     r0

           ghi     r8                  ; get device
           stxd                        ; write to stack
           glo     r8                  ; get high of lba
           stxd                        ; write to stack
           ghi     r7                  ; get mid of lba
           stxd                        ; write to stack
           glo     r7                  ; get lo of lba
           stxd                        ; write to stack
           ldi     1                   ; read one sector
           str     r2                  ; write to stack

           ghi     r3                  ; use rf for temporary pointer since
           phi     rf                  ; we'll overwrite it later anyway
           ldi     cftable.0           ; load with pointer to table
           plo     rf                  ; of ide register addresses to load

cfsetup:   sex     rf                  ; get value from ide address table
           out     cf_addr             ; output to address port
           sex     r2                  ; get argument data value from stack
           out     cf_data             ; output to data port
           ldn     rf                  ; if zero then at end of list
           bnz     cfsetup             ; loop back if not done

           sex     r2
           dec     r2

cfready:   inp     cf_data             ; read status register
           ani     ide_bsy             ; check if BSY set
           bnz     cfready             ; wait until its not

           ldn     r2                  ; get status value
           ani     ide_drq + ide_err   ; check if ERR or DRQ set
           bz      cfready             ; wait until either is

           ani     ide_err             ; check if ERR is set
           bnz     cferror2            ; jump if ERR is set

           sex     r3                  ; use inline value
           out     cf_addr             ; set sector counter for dma
           db      cf_count + 1        ; to one sector

           sex     r2                  ; dma enable value is on stack
           inc     r2
           out     cf_addr             ; enable appropriate dma direction
           dec     r2                  ; dma happens here, it's magic!

           sex     r3                  ; use inline value
           out     cf_addr             ; clear address register to
           db      ide_stat            ; disable dma, set status register

           sex     r2                  ; input to stack
cfcheck:   inp     cf_data             ; read status register
           xri     ide_rdy             ; check if RDY is set
           ani     ide_bsy + ide_rdy   ; and BSY is clear
           bnz     cfcheck             ; wait until they are

           ghi     r0                  ; update rf to point just past
           phi     rf                  ; transferred data as some code
           glo     r0                  ; such as the sys command relies
           plo     rf                  ; on this behavior writing sectors

           lda     r2                  ; get status register, pop stack
           adi     0                   ; clear df flag to indicate no error
           sep     r5                  ; return to caller

cferror1:  lda     r2                  ; get status register
           inc     r2                  ; pop the stack three times to

cfreturn:  inc     r2                  ; restore proper return position
           smi     0                   ; set df flag to indicate error
           sep     r5                  ; return to caller

cferror2:  ghi     r0                  ; update rf to point just past
           phi     rf                  ; transferred data as some code
           glo     r0                  ; such as the sys command relies
           plo     rf                  ; on this behavior writing sectors

           lda     r2                  ; get status register, discard dma
           br      cfreturn            ; return to caller setting df

cftable:   db      ide_coun            ; sector count
           db      ide_sect            ; sector start
           db      ide_cyll            ; cylinder low
           db      ide_cylh            ; cylinder high
           db      ide_head            ; head/device
           db      ide_cmnd            ; command
           db      0                   ; end of list


; SCRT call routing copied from BIOS and modified to recognize moved BIOS
; vector table for FF00 addresses. The page address of the new copied
; table is patched into this at ldipage+1 when the table is setup.

           sep     r3                  ; jump to called routine
newcall:   plo     re                  ; Save D
           ghi     r6                  ; save last R[6] to stack
           sex     r2
           stxd
           glo     r6
           stxd
           ghi     r3                  ; copy R[3] to R[6]
           phi     r6
           glo     r3
           plo     r6
           lda     r6                  ; get subroutine address
           phi     r3                  ; and put into r3
           adi     1
           bnz     nochange
ldipage:   ldi     0ffh
           phi     r3
nochange:  lda     r6
           plo     r3
           glo     re                  ; recover D
           br      newcall-1           ; transfer control to subroutine

end:       ; That's all folks!

