
;  VPRINTCH: Prints char directly to video memory
;  PARAM: dx holds the video mem address offset
;  PARAM: cx holds the char to print
vprintch:
  push ds		; Push the mem offset because we have to hack with it for drawing text in 16bit real mode (VGA mem address too big for 16bit)
  pusha			; Let's push all general registers to be clean
  mov bx, dx		; BX for mem pointers, dx is the offset for VGA mem
  mov ax, 0xb800	; 0xb8000 is too big, use 0xb800
  mov ds, ax		; Apply the curr mem offset to video mem
  mov ah, 0xf3		; Set display colors/fx of video text
  mov al, cl		; Get the char from cx to put in ax
  mov [bx], ax		; Put the full char info to video offset
  popa			; Let's fix it all back up
  pop ds		; If not, memory is whacked up
  ret

;  PRINTV: Prints to Video Memory
;  PARAM: ax is address of string to print
vprint:
  pusha
  mov bx, ax		; Because bx is actually used to reference mem pointer
  mov dx, 0		; We start with 0 for vid mem offset
  printv_loop:
    mov ax, [bx]	; Get char from current string mem address
    cmp al, 0		; Check for string terminator
    je printv_exit	; Exit if curr char is string terminator
    mov cx, ax		; Move the current char to cx param
    call vprintch	; param cx is the char to print
    add bx, 1		; Getting next char from string
    add dx, 2		; Increment to next video address
    jmp printv_loop
  printv_exit:
  popa			; Cleanup, cleanup... with dinosaurs...
  ret			; Return to sender


