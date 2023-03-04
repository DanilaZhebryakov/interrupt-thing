.model tiny
.code

org 100h

Start:
mov bx,0DDDDh
mov ds,bx
mov bx,0EEEEh
mov es,bx
mov ax, 1111h
mov bx, 2222h
mov cx, 3333h
mov dx, 4444h
mov bp,0BBBBh
mov si, 100
mov di, 0
Loop1:
 mov di, 0
 Loop2:
 dec di
 jnz Loop2
dec si
jnz Loop1
int 20h
end Start