include macros.asm

extrn free_mem:near

.model tiny
.code


    ; ReadDec: reads dechimal number from stdin
    ;    Entry: none
	;    Exit: ax = number
	;    Expects: ch = 0free_mem points to buffer
	;    Destroys: si, cx, dx, free_mem[0:10]
	;

    ; StrReadDec and StrReadNextDec: reads dechimal number from string at ds:si, up to cx chars.
    ;    Entry: ds:si = string to scan, cx = max chars
	;    Exit: ax = number bx increments each char, cx decrements
	;    Destroys: dx
	;    Note: non-existing number defaults to 0
    
    public StrReadNextDec
    StrReadNextDec proc 

        and cx, cx
        jnz @@chr_wait_loop
        ret

        @@chr_wait_loop:
            mov dl, ds:[si]   ;wait for dechimal char at ds:bx

            sub dl, '0'
            jb @@chr_wait_loop_end

            cmp dl, 9
            ja @@chr_wait_loop_end

            jmp StrReadDec

            @@chr_wait_loop_end:
            inc si
        loop @@chr_wait_loop
        ret
        endp

    public ReadDec
    ReadDec proc
        mov ah, 0ah ; buffered string input
        mov byte ptr free_mem, 6
        mov dx, offset free_mem
        int 21h ; dos service
        mov si, offset [free_mem + 2]
        mov cl, ds:[si]
        inc si

    public StrReadDec
    StrReadDec:
        xor ax, ax

        @@read_loop:
            xor dx, dx          ; clear dx, put new char - '0'
            mov dl, ds:[si]

            sub dl, '0'
            jb @@return

            cmp dl, 9
            ja @@return

            shl ax, 1   ; multiply ax by 10 in a really weird way
            add dx, ax  ; new char is already in dx
            shl ax, 2

            add ax, dx
            inc si
        loop @@read_loop

        @@return:
        ret
        endp

    ; converts hexchar to int. Returns out-of-range value if error
    ; Entry: dl = char
    ; Exit: dl = int
    public HexcharToInt
    HexcharToInt proc
        cmp dl, '0'
        jb @@hex
        cmp dl, '9'
        jg @@hex

        sub dl, '0'

        jmp @@return

        @@hex:
        and dl, 0DFh
        sub dl, 'A' - 10

        @@return:
        ret
        endp

    public StrReadNextHex
    StrReadNextHex proc
        and cx, cx
        jnz @@chr_wait_loop
        ret

        @@chr_wait_loop:
            mov dl, ds:[si]   ;wait for dechimal char at ds:bx
            call HexcharToInt
            cmp dl, 15
            ja @@chr_wait_loop_end

            jmp StrReadHex

            @@chr_wait_loop_end:
            inc si
        loop @@chr_wait_loop
        ret
        endp

    public StrReadHex
    StrReadHex proc
        
        xor ax,ax
        @@read_loop:
            xor dx, dx          ; clear dx, put new char - '0'
            mov dl, ds:[si]
            call HexcharToInt

            cmp dl, 15
            ja @@return

            shl ax, 4
            add al, dl

            inc si
        loop @@read_loop
        @@return:
        ret
        endp
end