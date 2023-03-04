    
include macros.asm

extrn free_mem:near

include output.inc
include btnstrct.inc

.model tiny
.code
    db "<buttons...>"

    ; Entry: al = btngroup
    ; Exit : bx = addr or 0 if not found
    ; Destroys: cx
    public GetFreeBtnAddr
    GetFreeBtnAddr proc
        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            cmp ds:[bx][btngroupid], 0
            jz @@return

            add bx, btn_struc_size
        loop @@btn_loop
        mov bx, 0
        @@return:
        mov ds:[bx][btngroupid], al
        ret
    endp

    ; structure as bytes on top of the stack (under return addr)
    ; Destroys: bx, cx
    public AddNewBtn
    AddNewBtn proc
        push bp
        mov bp, sp

        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            cmp ds:[bx][btngroupid], 0
            jz @@btn_move

            add bx, btn_struc_size
        loop @@btn_loop

        jmp @@return

        @@btn_move:

        mov di, bx

        mov bx, es
        push bx
        mov bx, ds
        mov es, bx

        mov bx, ds
        push bx
        mov bx, ss
        mov ds, bx
        
        lea si, [bp][4] ;2 bytes of return addr + 2 bytes of oldbp(stack grows down!)
        mov cx, btn_struc_size
        rep movsb

        pop bx
        mov ds, bx
        pop bx
        mov es, bx

        @@return:
        mov sp, bp
        pop bp
        ret btn_struc_size
    endp

    ; Remove button group
    ; Entry: al = group
    ; Destorys: cx, dx
    public RmBtnGroup
    RmBtnGroup proc
        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            cmp ds:[bx][btngroupid], al
            jnz @@loop_end
            mov byte ptr ds:[bx][btngroupid], 0
            mov word ptr ds:[bx][btntype], 0
            @@loop_end:
            add bx, btn_struc_size
        loop @@btn_loop
        ret
    endp

    ; Move button group
    ; Entry: al = group, si = dx, di = dy
    ; Destorys: cx, bx, dx
    public MvBtnGroup
    MvBtnGroup proc
        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            cmp ds:[bx][btngroupid], al
            jnz @@loop_end
            mov dx, si
            add byte ptr ds:[bx][btnx0], dl
            add byte ptr ds:[bx][btnx1], dl
            mov dx, di
            add byte ptr ds:[bx][btny0], dl
            add byte ptr ds:[bx][btny1], dl
            @@loop_end:
            add bx, btn_struc_size
        loop @@btn_loop
        ret
    endp


    ; called from mouse handler function
    ; sets ah to 0 if something done
    ; ah = event mask
    ; dl = x dh = y
    ; si = dx di = dy
    public BtnClickManager
    BtnClickManager proc
        enter 1,1
        mov [bp - 1], ah

        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            cmp dl, ds:bx[btnx0]
            jb @@loop_rel

            cmp dl, ds:bx[btnx1]
            ja @@loop_rel
 
            cmp dh, ds:bx[btny0]
            jb @@loop_rel

            cmp dh, ds:bx[btny1]
            ja @@loop_rel


            test ds:bx[btntype], 1;button enabled
            jz @@loop_end

            test ah, 2Ah ;button press
            push dx
            jz @@loop_rel
                
                mov al, ds:bx[btntype]
                and al, ah
                and al, 02Ah
                jz @@loop_end

                shr al, 1
                or al, 20h ;press event

                test ds:bx[btntype], 4 ;pressable
                jz @@nopress
                    or al, 2 ;pressed flag
                    inc btnpressedc
                @@nopress:
                or ds:bx[btnstate], al
                
                mov al, ds:bx[btntype]
                test al, 10h
                jz @@notogl
                    xor ds:bx[btnstate], 8 ;toggle state
                @@notogl:

                call ds:bx[btnhandler]   ;call event handler
                and ds:bx[btnstate], 0Ah ;clear events
                mov byte ptr ss:[bp-1], 0  ;set event used

            @@loop_rel:

                mov al, ds:bx[btntype]
                test ds:bx[btnstate], 2 ;not pressed
                jz @@loop_end

                shr ah, 1
                and al, ah
                and al, 02Ah
                jz @@loop_end
                shr al, 1
                or al, 40h ;release event
                or ds:bx[btnstate], al
                and ds:bx[btnstate], 0FCh ;clear pressed flag
                dec btnpressedc

                call ds:bx[btnhandler]   ;call event handler
                and ds:bx[btnstate], 0Ah ;clear events
                mov byte ptr ss:[bp-1], 0  ;set event used
            @@loop_end:
            add bx, btn_struc_size
            dec cx
        jz @@return
        jmp @@btn_loop

        @@return:
        mov ah, [bp-1]
        cmp ah, ah
        ;jz @@skip_out

        @@skip_out:
        leave
        ret
    endp

    ; called from mouse handler function
    ; ah = event mask
    ; dl = x dh = y
    ; si = dx di = dy
    public BtnDragManager
    BtnDragManager proc
        enter 1,1
        mov [bp - 1], ah

        mov cx, btn_count
        mov bx, offset screen_buttons
        @@btn_loop:
            test ds:bx[btnstate], 2 ;skip non-pressed
            jz @@loop_end

            test ds:bx[btntype], 40h ;skip

            jz @@loop_rel

            or ds:bx[btnstate], 80h  ;drag event
            call ds:bx[btnhandler]   ;call event handler
            and ds:bx[btnstate], 0Ah ;clear events
            mov byte ptr ss:[bp-1], 0; set event used

            @@loop_rel:
                cmp dl, ds:bx[btnx0]
                jb @@event_release

                cmp dl, ds:bx[btnx1]
                ja @@event_release
 
                cmp dh, ds:bx[btny0]
                jb @@event_release

                cmp dh, ds:bx[btny1]
                ja @@event_release

                jmp @@loop_end

                @@event_release:
                or ds:bx[btnstate], 40h ;release event
                and ds:bx[btnstate], 0FCh ;clear pressed flag
                dec btnpressedc

                call ds:bx[btnhandler]   ;call event handler
                and ds:bx[btnstate], 0Ah ;clear events
                mov byte ptr ss:[bp-1], 0; set event used

            @@loop_end:
            add bx, btn_struc_size
        loop @@btn_loop

        mov ah, [bp-1]
        leave
        ret
    endp


    ; debug procedure. Call everywhen from everywhere. Poops on screen.
    public Outbtns
    Outbtns proc
        push bp
        mov bp, sp
        pusha
        mov cx, 3
        setvmem
        mov bx, offset screen_buttons
        mov di, 160*12

        @@btn_loop:

            push cx

            mov dl, ds:bx[btntype]
            mov dh, ds:bx[btnstate]
            mov ah, 02h
            
            xor cx, cx
            push bx
            call WriteHex
            pop bx
            add di, 2

            mov dl, ds:bx[btnx0]
            mov dh, ds:bx[btny0]
            mov ah, 02h
            xor cx, cx
            push bx
            call WriteHex
            pop bx
            add di, 2

            mov dl, ds:bx[btnx1]
            mov dh, ds:bx[btny1]
            mov ah, 02h
            xor cx, cx
            push bx
            call WriteHex
            pop bx
            add di, 2

            xor dh, dh
            mov dl, ds:bx[btngroupid]
            mov ah, 02h
            xor cx, cx
            push bx
            call WriteHex
            pop bx
            add di, 2

            mov dx, ds:bx[btnhandler]
            mov ah, 02h
            xor cx, cx
            push bx
            call WriteHex
            pop bx

            add bx, btn_struc_size
            pop cx

            add di, 112
        loop @@btn_loop        

        popa
        mov sp, bp
        pop bp
        ret
    endp

    public btnpressedc 
    btnpressedc db 0

    public screen_buttons
    screen_buttons button 64 dup(<0,0>)
    db "<...buttons>"
end