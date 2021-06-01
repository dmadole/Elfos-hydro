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

biosvec    equ     03CEh
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

           db      6+80h              ; month
           db      1                  ; day
           dw      2021               ; year
           dw      3                  ; build
           db      'Written by David S. Madole',0

minvers:   db      0,3,1              ; minimum kernel version needed


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for himem variable to allocate
           ; memory for the persistent module to use.

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
           bnz     checkvec           ; positive, running > minimum, so pass

           dec     rf                 ; zero, so equal, keep checking
           glo     rf
           bnz     versloop           ; if we exit this versions are same


           ; Check if we are able to shim BIOS, either because we are the
           ; first to do so, or because another module that already has done
           ; so set biosvec to point to the table it already installed.

checkvec:  ldi     biosvec.1
           phi     rb
           ldi     biosvec.0
           plo     rb

           ghi     r4                 ; if BIOS vector is still in ROM
           smi     0f8h               ; then continue installing
           bdf     allocmem

           ldn     rb                 ; otherwise fail unless there is a
           adi     1                  ; new table pointed to by biosvec
           shr                        ; this tests for either 00 or FF
           lbz     hookfail           ; as either could mean uninitialized
           

           ; Allocate memory below himem for the driver code block, leaving
           ; address to copy code into in register R8 and R9 and length
           ; of code to copy in RF. Updates himem to reflect allocation.

allocmem:  ldi     (end-module).1     ; get length of code to install
           phi     rf
           ldi     (end-module).0
           plo     rf

           ldi     himem.1            ; pointer to top of memory variable
           phi     r7
           ldi     himem.0
           plo     r7

           sex     r7                 ; subtractions reference himem

           inc     r7                 ; move to lsb of himem
           glo     rf                 ; subtract size to install from himem
           sd                         ; keep borrow flag of result
           ldi     0                  ; but round down to page boundary
           plo     r8
           plo     r9

           dec     r7                 ; move to msb of himem and finish
           ghi     rf                 ; subtraction to get code block address
           sdb
           phi     r8
           phi     r9

           dec     r8                 ; set himem to one less than block

           ghi     r8                 ; update himem to below new block
           str     r7
           inc     r7
           glo     r8
           str     r7
           dec     r7

           inc     r8                 ; restore to start of code block


           ; Copy the code of the persistent module to the memory block that
           ; was just allocated. R8 and R9 both point to this block before
           ; the copy. R9 will be used but R8 will still point to it after.

           ldi     module.1           ; get source address to copy from
           phi     ra
           ldi     module.0
           plo     ra

copycode:  lda     ra                 ; copy code to destination address
           str     r9
           inc     r9
           dec     rf
           glo     rf
           bnz     copycode
           ghi     rf
           bnz     copycode


           ; If there is already a BIOS vector page allocated from a prior
           ; module installation, set R9 to point to it.

           ldn     rb                 ; check is there is a bios vector
           adi     1                  ; already, otherwise install one
           shr                        ; this tests for either 00 or FF
           bz      allocvec           ; as either could mean uninitialized

           lda     rb                 ; if non-zero, set into r9
           phi     r9
           ldn     rb
           plo     r9

           br      patching           ; go patch the routines we need to


           ; Otherwise, get a page of memory for a new BIOS vector table.
           ; Since we already adjusted himem to just below a page boundary
           ; this is simple to do. Copy the page from FF00 into the new table
           ; and leave R9 pointing to it.

allocvec:  ldn     r7                 ; get msb of himem which will be
           phi     r9                 ; xxff so is same as start of block
           str     rb                 ; save into msb of biosvec
           smi     1                  ; reduce himem by one memory page
           str     r7

           ldi     0                  ; new block starts on page boundary
           plo     r9
           inc     rb                 ; save into lsb of biosvec
           str     rb

           plo     ra                 ; point to BIOS at FF00
           ldi     0ffh
           phi     ra

copyvec:   lda     ra                 ; copy the whole page contents
           str     r9
           inc     r9
           glo     r9
           bnz     copyvec            ; loop until lsb wraps to zero

           ghi     r9                 ; adjust back to start of page
           smi     1
           phi     r9


           ; If we allocated a new vector table, we need to put the address
           ; of it into the replacement CALL routine in the module code,
           ; and then change R4 to point to that new CALL routine.

           glo     r8                  ; get address of ldi instruction
           adi     (ldipage-module).0
           plo     ra
           ghi     r8
           adci    (ldipage-module).1
           phi     ra

           inc     ra                  ; point to ldi argument and set
           ghi     r9
           str     ra

           glo     r8                  ; calculate address of copied call
           adi     (newcall-module).0  ; routine and update into r4
           plo     r4
           ghi     r8
           adci    (newcall-module).1
           phi     r4


           ; Update kernel and BIOS hooks to point to our module code. At
           ; this point, R9 points to the new BIOS jump table in RAM, and
           ; R8 points to the base address of the module code in RAM.

patching:  ldi     patchtbl.1        ; Get point to table of patch points
           phi     r7
           ldi     patchtbl.0
           plo     r7

           sex     r7                 ; add instructions will use table

ptchloop:  lda     r7                 ; a zero marks end of the table
           bz      ptchdone

           phi     ra                 ; save msb of address but check if
           smi     0ffh               ; it's a bios ff00 vector, if it's
           bnz     isntffxx           ; not then use as-is

           ghi     r9                 ; if the address is ffxx replace it
           phi     ra                 ; with equivalent in the copy in RAM

isntffxx:  lda     r7                 ; get lsb of patch address
           plo     ra
           inc     ra                 ; skip the lbr opcode

           inc     r7                 ; point to lsb of both addresses
           inc     ra
           glo     r8                 ; add the offset in the table to the
           add                        ; base address in RAM and update the
           str     ra                 ; address at the patch point

           dec     r7                 ; point to msb of both addresses
           dec     ra
           ghi     r8                 ; same as above for the msb
           adc
           str     ra

           inc     r7                 ; point to next entry in table and
           inc     r7                 ; continue until all are done
           br      ptchloop


           ; At this point we are done, output a success message and end.

ptchdone:  sex     r2                 ; put stack back to r2 and push
           ldi     success.1          ; address of success message to print
           stxd
           ldi     success.0
           stxd

           lbr     output             ; output copyright plus success

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

message:   db      'Hydro Compact Flash DMA Driver Build 3 for Elf/OS',13,10,0
success:   db      'Copyright 2021 by David S Madole',13,10,0
vermsg:    db      'ERROR: Needs kernel version 0.3.1 or higher',13,10,0
hookmsg:   db      'ERROR: SCALL is already diverted from BIO','S',13,10,0


           ; Table giving addresses of jump vectors we need to update, along
           ; with offset from the start of the module to repoint those to.

patchtbl:  dw      f_ideread, cfread - module
           dw      f_idewrite, cfwrite - module
           db      0

           org     $ + 0ffh & 0ff00h

module:    ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.


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


; SCRT call routine mostly copied from BIOS and modified to recognize moved
; BIOS vector table for FF00 addresses. The page address of the new copied
; table is patched into this at ldipage + 1 when the table is installed.

sepcall:   sep     r3                  ; leaves r4 pointing to newcall

newcall:   plo     re                  ; save d register
           sex     r2                  ;  make sure x is 2

           ghi     r6                  ; save r6 to stack
           stxd
           glo     r6
           stxd

           ghi     r3                  ; copy r3 into r6
           phi     r6
           glo     r3
           plo     r6

           lda     r6                  ; copy subroutine address
           phi     r3                  ;  high byte into r3.1

           smi     0ffh                ; is the subroutine page address ff?
           bnz     nochange            ;  if not, leave as it is

ldipage:   ldi     0ffh                ; if yes, update with relocated page
           phi     r3                  ;  which is patched into here

nochange:  lda     r6                  ; copy subroutine address
           plo     r3                  ;  low byte into r3.0

           glo     re                  ; recover saved d, then
           br      sepcall             ;  jump to subroutine via sep


end:       ; That's all folks!

