
.286
locals @@

exit0 macro
    mov ax, 4c00h ;exit(0)
    int 21h
    endm


        ; Output $ -terminated string
        ;   Entry: DS:DX - string
        ;   Exit: none
        ;   Destroys: ax
        ;
console_outstr macro
    mov ah, 09h
    int 21h
    endm

setvmem macro
    mov bx, 0B800h ;video memory segment
    mov es, bx
    endm
setvmema macro
    mov ax, 0B800h ;video memory segment
    mov es, ax
    endm
displmouseptr macro
    mov ax, 01h
    int 33h
    endm
hidemouseptr macro
    mov ax, 02h
    int 33h
    endm