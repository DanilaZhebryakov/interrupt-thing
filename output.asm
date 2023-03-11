include macros.asm

extrn free_mem:near

.model tiny
.code

        ;
        ; writes number on screen in binary
        ;   Entry: es:di - place on screen, dx = number, ah = color attr, df = 0 for normal write
        ;   Exit: none
        ;   Expects: es = video memory segment
        ;   Destroys: al, cx
        ;
        public WriteBin
        WriteBin proc
    	    mov cx, 16
            @@zero_cut_loop:
                rol dx, 1
                test dx, 1 ;skip leading 0s
            loopz @@zero_cut_loop
            inc cx
            jmp @@write_loop_start
            
            @@write_loop:
		        rol dx, 1
                @@write_loop_start:
		        mov al, 1
		        and al, dl
		        add al, '0'
                stosw
            loop @@write_loop
            ret
            endp
	    ;
        ; writes number on screen in hex
        ;   Entry: es:di - place on screen, dx = number, ah = color attr, df = 0 for normal write
        ;   Exit: none
        ;   Expects: es = video memory segment
        ;   Destroys: al, bx, cx
        ;
        public WriteHex
        WriteHex proc
	        mov cx, 4
            @@write_loop:
		        rol dx, 4
		        mov bx, 0Fh
		        and bx, dx
		        mov al, cs:@@hexchar[bx]
                stosw 
            loop @@write_loop
            ret
	        @@hexchar db "0123456789ABCDEF"
            endp
	
	;
	; writes number to screen in dechimal (UNSIGNED)
	;    Entry: es:di - place on screen, ax = number, df = 0 for normal write
	;    Exit: none
	;    Expects: es = video memory segment
	;    Destroys: ax, cx, dx, si
	;
    public WriteDec
	WriteDec proc
        mov si, 10 ; base 10
        add di, 10 ; dechimals are written right-to-left, so need to move cursor
        mov cx, 5
        @@write_loop:
            xor dx, dx
            div si
            sub di, 2
            add dl, '0'
            mov es:[di], dl
        loop @@write_loop
        add di, 10
        ret
        endp
end