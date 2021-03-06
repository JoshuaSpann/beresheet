; This was written for FASM, may need adjustments for NASM

org 0x7c00		; Set the base address offset for bootloader
jmp short start
nop

; For defining a FAT filesystem/volume
DiskLabel		db "BERESHEET"	; What the disk is called
BytesPerSector		dw 512		; 1 sector is 512 bytes long
SectorsPerCluster	db 1		; 1 cluster is 1 sectors
BootRecdReservedSectors	dw 2		; 2 reserved because we load stage_2
NumberFATs		db 2		; Number of FAT tables (2 standard)
RootDirEntries		dw 224		; Max number items in root dir (for floppy disks)
NumLogicalSectors	dw 2880		; Total sectors in disk (2880 in floppy)
MediumDescriptiorByte	db 0x0f0	; Disk info, bits: 0=sides/heads(0=single,1=double); 1=size(0=9sectorsPerFat,1=8sectorsPerFat); 2=density(0=80tracks,1=40tracks); 3=type(0=fixedDisk/hdd,1=removable/floppy); 4-7=unused(always val of 1)
SectorsPerFat		dw 9		; Actual sectors per fat (like bit 1); bit 1 is set to 9 so we define that here
SectorsPerTrack		dw 18		; 18 sectors per track on floppy
NumSides		dw 2		; 2 heads represent 1 cylinder
NumHiddenSectors	dd 0		; Number sectors from strt of disk and start of volume
NumLargeSectors		dd 0		; Number of big/huge lba sectors
DriveNum		dw 0		; Floppy is drive 0
DriveSignature		db 41		; 41 is for ms-dos floppy
VolumeId		dd 0x00000000	; Serial number assigned by util that formats the disk, unique to disk
VolumeLabel		db "BERESHEET  "; 11 chars total, absolute
FileSystem		db "FAT12   "	; 8 chars total, absolute!!!



start:
call screen_clear_bios
call printos

input_loop:
  mov ax, '>'		; printc param: Char to print (ASCII value)
  call printc_bios	; Let's print the char in ax!

  mov ax, 0x9000	; read param: address to read to
  mov bx, 13		; read param: what terminates read input (enter key)
  call read_bios	; Let's read data until the char in bx is pressed

  mov bx, cmd_cls	; strcmp param: second comparison string address (first string's address in ax already)
  call strcmp_bios	; Compare string at addresses ax to string at address bx
  cmp dx, 0		; If strings match
  je clear_screen

  mov bx, cmd_load
  call strcmp_bios
  cmp dx, 0
  je load_stage2

  mov bx, cmd_os
  call strcmp_bios
  cmp dx, 0
  jne input_loop
  call printos
jmp input_loop		; Let's make it like a terminal

print_invalid_command_msg:
  mov ax, msg_cmd_invalid
  call print_bios	; Prints message string from address in ax
  jmp input_loop	; Sloppy return to main loop

clear_screen:
  call screen_clear_bios		; BIOS way of clearing screen
  jmp input_loop	; Sloppy return to main loop

jmp $			; Forever jump to the address of the current position before emitting the bytes

load_stage2:
  mov ax, 2
  mov bx, stage_2
  call load_sector_bios

; PARAM: ax holds the sector number to load
; PARAM: bx holds the proc/address to load
load_sector_bios:
  push bx
  push ax
  mov al, 0x00		; Reset floppy disk
  mov dl, 0x00		; Drive to reset
  int 0x13		; BIOS call disk
  mov ah, 0x02		; Read sector
  mov al, 1		; # sectors to read
  mov dl, 0x80		; Unnecessary?
  mov ch, 0		; Cylinder num
  mov dh, 0		; Head number
  pop bx		; Load the sector number to start op at
  mov cl, bl		; Starting sector number
  pop bx		; bx is Where to load to ; mov bx, stage_2
  int 0x13		; BIOS Call disk
  jmp bx		; We have to actually jump there

;----------------;

; Variables ;

os: db 'beresheet',0	; An unimportant string that we can overwrite
cmd_cls: db 'cls',0
cmd_load: db 'load',0
cmd_os: db 'os',0
cmd_tst: db 'tst',0
msg_cmd_invalid: db 'Invalid Command',0

;----------------;

; PROCEDURES ;

printos:
  pusha
  mov ax, os		; print param: the address of the string to print
  call print_bios	; Let's print until we find a string terminator (0)!
  popa
  ret

; PARAM: ax holds address of first string to compare
; PARAM: bx holds address of second string to compare
; RETURN:dx holds status flag of procedure after execution
; Compares 2 strings in memory addresses stored in ax and bx, returning 0 for match and 1 for diff in dx
strcmp_bios:
  mov [0x7ea0], ax	; It sorta makes sense to store ax in an address with a
  mov [0x7eb0], bx	; It sorta makes sense to store bx in an address with b
  pusha			; Keep it clean
  mov cx, 0		; Use the counter for mem address offset
  strcmp_bios_loop:
    mov ax, [0x7ea0]	; Get the first string base address and move to ax
    add ax, cx		; Apply offset to mem address stored in ax
    mov bx, ax		; We need bx to access mem location via pointer
    mov al, [bx]	; Get char from address to al
    mov bx, [0x7eb0]	; Get the second string base address and move it to bx
    add bx, cx		; Apply offset to mem address stored in bx
    mov bl, [bx]	; Get char from 2nd address to bl
    cmp al, bl		; See if the chars match
    jne strcmp_bios_exit_bad	; No match leaves with exit 1, else continues
    cmp al, 0		; Is al string terminator?
    je strcmp_bios_exit_ok	; If al is string terminator, bl is too so exit 0
    add cx, 1		; Increment the counter
  jmp strcmp_bios_loop	; Continue looping until an end is reached!
  strcmp_bios_exit_bad:
    mov dx, 1		; Move 1 to dx to show strings don't match
    jmp strcmp_bios_exit; Skip to exit
  strcmp_bios_exit_ok:
    mov dx, 0		; Move 0 to dx to show strings do match
  strcmp_bios_exit:
    mov [0x7f00], dx	; dx is the return address for all procedures right now
  popa
  mov dx, [0x7f00]	; Set dx to the proper value
  ret

; PARAM: ax holds address of string to print
; CALLS: printl for \r\n\n clean line print
print_bios:
  pusha			; Let's do this cleanroom style!
  mov [0x7f00], ax	; We use this mem address to hold the actual address to print from
  mov ah, 0x0e		; When we call BIOS int 10h, it knows to output char
  mov cx, 0		; Counter to offset what char in mem address to print from
  print_loop:
    mov bx, [0x7f00]	; bx is used to hold address we point to get vals from
    add bx, cx		; Offset the address by the counter to get the correct char to print
    mov al, [bx]	; al is value to print with BIOS int 10h, mov char directly from mem
    cmp al, 0		; Is the current char a string terminator (\0)?
    je print_exit	; If so, leave. If not, ignored
    add bx, 0x00ff	; This is for fun, a mem offset to hold copy of what's printed last
    mov [bx], al	; Push the current char to mem location with copy of string
    int 0x10		; Let's print the curr char!
    add cx, 1		; Increment counter so we don't print the first letter forevermore!
    jmp print_loop	; Move along to the next character
  print_exit:		; The escape point for the above loop
  call printl_bios		; Newlines automatically for being purdy
  popa			; Let's return things to the way they were before we mucked around with the registers
  ret			; Return to call point

; PARAM: ax holds char to print
printc_bios:
  mov [0x8000], ax	; Why not use a memory address for fun? Each address is a byte long and a char is a byte... Unnecessary? Sure.
  pusha			; Cleanliness is next to godliness
  mov al, [0x8000]	; Let's get the value at the address
  mov ah, 0x0e		; With BIOS int 10h it means PRINT CHAR!
  int 10h		; Print char in al
  popa			; Okay, clean up our mess
  ret			; Back to sender

; Simply does a \r\n\n for cleaner line print
printl_bios:
  pusha		; Is this "prepping"?
  mov ah, 0x0e	; Newline
  mov al, 0x0d	; Carriage return (like the Home key)
  int 10h	; Move cursor to the beginning of the line
  mov al, 0x0a	; New line feed
  int 0x10	; Print line feed
  int 0x10	; Print again for readability!
  popa		; Does this count as OCD?
  ret		; Go home, Dorothy

; PARAM: ax is the memory address to read input to
; PARAM: bx is the terminator to stop reading input
read_bios:
  mov [0x7f00], ax	; Let's store ax's value (another mem address) at this address in memory
  pusha			; Sloppy cleanrooming
  mov dx, bx		; We want to have bx free for memory addressing so dx holds the value to terminate reading
  read_bios_reset:
  mov bx, [0x7f00]	; Let's get the memory address to store the char read to
  read_bios_loop:
    mov ah, 0		; ah 0 with BIOS int 16h means get keystroke from keyboard
    int 16h		; Let's get the keystroke (value is stored in al)
    mov [bx], al	; Let's put the char value to the address in mem
    cmp al, dl		; Is the keystroke is our read terminator (dx)?
    je read_bios_exit	; If so, exit. If no, go on...
    add bx, 1		; Let's move to the next address to store next char
    call printc_bios	; Let's print the current char (already setup for us, printc sets ah for us to print properly)
    cmp al, 8		; Was the key backspace?
    je read_bios_backspace	; If so we shall overwrite current character (need to decrement or bug)
    jmp read_bios_loop	; Continue until we reach the terminator
  read_bios_backspace:
    sub bx, 2		; We go to the last char address in mem
    cmp bx, [0x7f00]	; Is the address in bx < the original address given?
    jl read_bios_reset	; Start from the top if so, that way we don't screw up other mem locations
    jmp read_bios_loop	; Default if the current address >= the original address given
  read_bios_exit:		; The exit point for the above loop
    mov ax, 0		; Let's terminate that string properly for future use
    mov [bx], ax	; Let's put that in memory so the string is complete
  call printl_bios		; Pretty print after the terminator has been made!
  popa			; Or are we just covering our tracks all along?!
  ret			; Go talk to mom about it

; Clears the Screen via BIOS
screen_clear_bios:
  pusha
  mov ah, 0
  mov al, 0x03
  int 10h
  popa
  ret

;----------------;

; This is a purdy bootloader!

times 510-($-$$) db 0	; Let's do 0s because no partitions or FS right now
dw 0xaa55		; Magic number to say "Hi, I'm Bootsector!"

;-----------------;
;  SECOND SECTOR  ;
;-----------------;

stage_2:

mov ax, msg_stage2
call print_bios

call shell

include 'video.asm'
mov ax, msg_stage2
call vprint

use16
mov ax, cmd_exit
call print_bios

cli
hlt

;  Minimalist shell better than the one at the start  ;

shell:
  pusha
  shell_loop:
    mov ax, 4
    mov bx, 8
    call vcursor_move

    mov ax, 0x9000	; Load last command to vprint
    call vprint		; Print to video

    call prompt

    mov bx, cmd_exit	; Check for 'exit' command
    call strcmp_bios	; Perform check
    cmp dx, 0		; Is the cmd 'exit'?
    je shell_exit	; Exit safely if so

    ; Clear Screen Command (BIOS)
    mov bx, cmd_cls
    call strcmp_bios
    cmp dx, 0
    je screen_clear

    ; Warm Reboot Command (BIOS)
    mov bx, cmd_reboot
    call strcmp_bios
    cmp dx, 0
    je reboot_warm_bios

  jmp shell_loop
  shell_exit:
  popa
  ret

prompt:
    mov ax, '>'		; printc param: Char to print (ASCII value)
    call printc_bios	; Let's print the char in ax!
    mov ax, 0x9000	; read param: address to read to
    mov bx, 13		; read param: what terminates read input (enter key)
    call read_bios	; Let's read data until the char in bx is pressed
    ret

screen_clear:
  call screen_clear_bios
  jmp shell_loop

reboot_warm_bios:
  int 0x19


cmd_exit db 'exit',0
cmd_reboot db 'reboot',0
msg_stage2 db 'LOADED STAGE 2!',0
msg_success db ':)',0

;times 512-($-$$) db 0	; Let's do 0s because no partitions or FS right now
;dw 0xaa55		; Magic number to say "Hi, I'm Bootsector!"
