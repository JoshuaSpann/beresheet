; This was written for FASM, may need adjustments for NASM
org 0x7c00		; Set the base address offset for bootloader

mov ax, 98		; printc param: Char to print (ASCII value)
call printc		; Let's print the char in ax!

mov ax, os		; print param: the address of the string to print
call print		; Let's print until we find a string terminator (0)!

input_loop:
 mov ax, 0x9000		; read param: address to read to
 mov bx, 'z'		; read param: what terminates read input
 call read		; Let's read data until the char in bx is pressed
 call print		; Since we want to print and ax is also the address to print, we can skip moving the address again and just call it
jmp input_loop		; Let's make it like a terminal

jmp $			; Forever jump to the address of the current position before emitting the bytes

;----------------;

; Variables ;

os: db 'beresheet',0	; An unimportant string that we can overwrite

;----------------;

; PROCEDURES ;

; PARAM: ax holds address of string to print
; CALLS: printl for \r\n\n clean line print
print:
  pusha			; Let's do this cleanroom style!
  mov [0x7f00], ax	; We use this mem address to hold the actual address to print from
  mov ah, 0x0e		; When we call BIOS int 10h, it knows to output char
  mov cx, 0		; Counter to offset what char in mem address to print from
  print_loop:
    mov bx, [0x7f00]	; bx is used to hold address we point to get vals from
    add bx, cx		; Offset the address by the counter to get the correct char to print
    mov al, [bx]	; al is value to print with BIOS int 10h, mov char directly from mem
    add bx, 0x00ff	; This is for fun, a mem offset to hold copy of what's printed last
    mov [bx], al	; Push the current char to mem location with copy of string
    cmp al, 0		; Is the current char a string terminator (\0)?
    je print_exit	; If so, leave. If not, ignored
    int 0x10		; Let's print the curr char!
    add cx, 1		; Increment counter so we don't print the first letter forevermore!
    jmp print_loop	; Move along to the next character
  print_exit:		; The escape point for the above loop
  call printl		; Newlines automatically for being purdy
  popa			; Let's return things to the way they were before we mucked around with the registers
  ret			; Return to call point

; PARAM: ax holds char to print
printc:
  mov [0x8000], ax	; Why not use a memory address for fun? Each address is a byte long and a char is a byte... Unnecessary? Sure.
  pusha			; Cleanliness is next to godliness
  mov al, [0x8000]	; Let's get the value at the address
  mov ah, 0x0e		; With BIOS int 10h it means PRINT CHAR!
  int 10h		; Print char in al
  popa			; Okay, clean up our mess
  ret			; Back to sender

; Simply does a \r\n\n for cleaner line print
printl:
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
read:
  mov [0x7f00], ax	; Let's store ax's value (another mem address) at this address in memory
  pusha			; Sloppy cleanrooming
  mov dx, bx		; We want to have bx free for memory addressing so dx holds the value to terminate reading
  mov cx, 0		; Yet ANOTHER counter to offset where the current char is stored in memory
  read_loop:
    mov ah, 0		; ah 0 with BIOS int 16h means get keystroke from keyboard
    int 16h		; Let's get the keystroke (value is stored in al)
    cmp al, dl		; Is the keystroke is our read terminator (dx)?
    je read_exit	; If so, exit. If no, go on...
    mov bx, [0x7f00]	; Let's get the memory address to store the char read to
    add bx, cx		; Let's apply the offset so we dont overwrite the 1st char
    mov [bx], al	; Let's put the char value to the address in mem
    add cx, 1		; Moving on... (to the next address)
    ;mov ax, al
    call printc		; Let's print the current char (already setup for us, printc sets ah for us to print properly)
    jmp read_loop	; Continue until we reach the terminator
  read_exit:		; The exit point for the above loop
    add bx, cx		; Let's do one more address in memory because this is a string of text we have been reading
    mov ax, 0		; Let's terminate that string properly for future use
    mov [bx], ax	; Let's put that in memory so the string is complete
  call printl		; Pretty print after the terminator has been made!
  popa			; Or are we just covering our tracks all along?!
  ret			; Go talk to mom about it

;----------------;

; This is a purdy bootloader!

times 510-($-$$) db 0	; Let's do 0s because no partitions or FS right now
dw 0xaa55		; Magic number to say "Hi, I'm Bootsector!"
