include macros.asm

extrn free_mem:near

.model tiny
.code
        ;
        ; Clear the screen
        ;   Entry: AH = color attr, AL = char
        ;   Exit: none
        ;   Expects: es = video memory segment
        ;   Destroys: cx, bx
        ;
        public ClrScr
        ClrScr proc
            xor bx,bx
            mov cx, 80 * 25

            clrscr_loop:
            mov es:[bx], ax
            add bx, 2
            loop clrscr_loop
            ret
            endp

        ;
        ; move string on screen
        ;   Entry: ds:si - source cx = len, es:di - place on screen, ah = color attr
        ;   Exit: di = end of output
        ;   Expects: es = video memory segment
        ;   Destroys: al, cx, si, di
        ;
        public OutStr
        OutStr proc
            and cx,cx
            jz outstr_end
            inc si
            outstr_loop:
                mov al, ds:[si]
                mov es:[di], ax
                inc si
                add di, 2
            loop outstr_loop

            outstr_end:
            ret
            endp

        ;
        ; draw frame on screen
        ;   Entry: ah = color attr ,es:di - place on screen, ds:bx = charset addr, dh = height, dl = width, si = screen width (bytes) - 4
        ;   Exit: none
        ;   Expects: es = video memory segment ch = 0
        ;   Destroys: al, cx, di
        ;
        public OutFrm
        OutFrm proc
            call @@fillstr
            
            mov cl, dl ;next line
            sub di, cx
            sub di, cx
            add di, si

            add bx, 3
            @@outfrm_loop:
                call @@fillstr
                mov cl, dl ;next line
                sub di, cx
                sub di, cx
                add di, si
                dec dh
            jnz @@outfrm_loop

            add bx, 3
            call @@fillstr


            ret
            @@fillstr:
            mov al, ds:[bx]
            stosw
            mov al, ds:[bx][1]
            mov cl, dl
            rep stosw
            mov al, ds:[bx][2]
            stosw
            ret
            endp

        ;
        ; fill given rectangle
        ;   Entry: es:di - place on screen, ax = color attr + char, dh = height, dl = width, si = screen width
        ;   Exit: di - bellow bottom left corner of filled rect
        ;   Expects: ch = 0, df = 0
        ;   Destroys: cx, dh, di
        ;
        public FillRect
        FillRect proc
            @@fill_loop_b:
                mov cl, dl ;counter for char write (in words)
                rep stosw
                mov cl, dl ;cx = 0 now, use it to expand width to dword
                shl cx, 1  ;one char = word ,so x2 
                sub di, cx ;carriage return
                add di, si ; one row down
                
                dec dh
            jge @@fill_loop_b
            ret
            endp

        ; Writes data from buffer to screen (or backwards)
        ; Entry: ds:si - from, es:di - to, ax = src row size, bx = dst row size, dh = height, dl = width
        ; Exit: si and di move to bottom left corner, dh = 0
        ; Expects: ch = 0, df = 0
        ; Destroys: cx(!=0), dh, di

        public WriteBuffer
        WriteBuffer proc
            @@fill_loop_b:
                mov cl, dl ;counter for char write (in words)
                rep movsw
                mov cl, dl ;cx = 0 now, use it to expand width to dword
                shl cx, 1  ;one char = word ,so x2 

                sub di, cx ;carriage return
                sub si, cx

                add di, bx ; one row down
                add si, ax

                dec dh
            jnz @@fill_loop_b
            ret
        endp

        ; Updates saved buffer with changes on screen
        ; Entry: ds:si - drawing buffer, es:di - screen, bx - offset of saved-buffer relative to drawing buffer, ax = src row size, dh = height, dl = width
        ; Exit: si and di move to bottom left corner, dh = 0
        ; Expects: ch = 0, df = 0
        ; Destroys: cx(!=0), dh, di
        public UpdSaveBuffer
        UpdSaveBuffer proc
            @@fill_loop_b:

                mov cl, dl ;counter for char write (in words)
                @@row_loop:
                repe cmpsw
                je @@nextrow
                
                push cx
                mov cx, es:[di-2]
                mov ds:[bx][si-2], cx ;move char+attr into saved buffer
                pop cx
                jcxz @@nextrow
                jmp @@row_loop

                @@nextrow:
                mov cl, dl ;cx = 0 now, use it to expand width to dword
                shl cx, 1  ;one char = word ,so x2 

                sub di, cx ;carriage return
                sub si, cx

                add di, 160 ; one row down
                add si, ax

                dec dh
            jnz @@fill_loop_b
            ret
        endp
end