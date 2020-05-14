org 0x7c00

mov ax, 98
call printc

mov ax, os
call print

input_loop:
 mov ax, 0x9000
 mov bx, 'z'
 call read
 call print
jmp input_loop

jmp $

os: db 'beresheet',0
;pointer_address: db 0x7f00

; PARAM: ax holds address of string to print
; CALLS: printl for \r\n\n clean line print
print:
  pusha
  mov [0x7f00], ax
  mov ah, 0x0e
  mov cx, 0
  print_loop:
    mov bx, [0x7f00]
    add bx, cx
    mov al, [bx]
    add bx, 0x00ff
    mov [bx], al
    cmp al, 0
    je print_exit
    int 0x10
    add cx, 1
    jmp print_loop
  print_exit:
  call printl
  popa
  ret

; PARAM: ax holds char to print
printc:
  mov [0x8000], ax
  pusha
  mov al, [0x8000]
  mov ah, 0x0e
  int 10h
  popa
  ret

; Simply does a \r\n\n for cleaner line print
printl:
  pusha
  mov ah, 0x0e
  mov al, 0x0d
  int 10h
  mov al, 0x0a
  int 0x10
  int 0x10
  popa
  ret

; PARAM: ax is the memory address to read input to
; PARAM: bx is the terminator to stop reading input
read:
  mov [0x7f00], ax
  pusha
  mov dx, bx
  mov cx, 0
  read_loop:
    mov ah, 0
    int 16h
    cmp al, dl
    je read_exit
    mov bx, [0x7f00]
    add bx, cx
    mov [bx], al
    add cx, 1
    ;mov ax, al
    call printc
    jmp read_loop
  read_exit:
    add bx, cx
    mov ax, 0
    mov [bx], ax
  call printl
  popa
  ret

; This is a purdy bootloader!
times 510-($-$$) db 0
dw 0xaa55
