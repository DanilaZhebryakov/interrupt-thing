include macros.asm

extrn free_mem:near

include output.inc
include buttons.inc
include windows.inc

extrn BtnClickManager:near
extrn BtnDragManager:near
extrn btnpressedc:near
extrn regs_win:near
extrn UpdRegsWin:near

.model tiny
.code
    db "<events....>"
    ; ax = event mask
    ; bx = btn state
    ; cx = x
    ; dx = y
    ; si = dx
    ; di = dy
    ; ds = mouse driver segment

    mouse_old_x dw 0
    mouse_old_y dw 0

    MouseHandler proc
        enter 12, 1
        @@mouse_x     equ   ss:[bp-2 ]
        @@mouse_y     equ   ss:[bp-4 ]
        @@mouse_dx    equ   ss:[bp-6 ]
        @@mouse_dy    equ   ss:[bp-8 ]
        @@mouse_event equ   ss:[bp-10]
        @@mouse_btn   equ   ss:[bp-12]

        mov @@mouse_x, cx
        mov @@mouse_y, dx
        mov @@mouse_dx, si
        mov @@mouse_dy, di
        mov @@mouse_event, ax
        mov @@mouse_btn, bx


        mov bx, ds ; save ds
        push bx
        mov bx, cs ;new ds = cs
        mov ds, bx


        ; write cursor pos on screen (for debug reasons)
        mov bx, 0B800h ;video memory segment
        mov es, bx

        mov dx, cx
        xor di, di
        mov ah, 08h
        call WriteHex
        mov dx, @@mouse_y
        add di, 2
        call WriteHex

        mov ax, dx
        shr ax, 3
        sub ax, mouse_old_y
        mov di, ax

        mov cx, @@mouse_x

        mov ax, cx
        shr ax, 3
        sub ax, mouse_old_x
        mov si, ax

        mov ax, @@mouse_event
    

        shl dx, 5 ; correct value in dh
        shr cx, 3
        mov dl, cl ;pos format
        mov ah, al ;handler-specific place for mask

        test ah, 007eh ;button events
        jz @@btnskip
            push dx
            call BtnClickManager ;ah is saved by function
            pop dx
        @@btnskip:


        test ah, 1 ;drag
        jz @@btndragskip

        cmp byte ptr btnpressedc, 0
        jz @@btndragskip
            push dx
            call BtnDragManager ;ah is saved by function
            pop dx
        @@btndragskip:
        mov byte ptr mouse_old_x, dl
        mov byte ptr mouse_old_y, dh

        pop bx ;restore ds value from stack
        mov ds, bx
    
        cmp ah, 0
        jz @@return

        mov ax, @@mouse_event
        and ax, old_mouse_event_mask
        jz @@return

        mov bx, @@mouse_btn
        mov cx, @@mouse_x
        mov dx, @@mouse_y
        mov si, @@mouse_dx
        mov di, @@mouse_dy
        leave

        retf ; mouse handler chaining disabled
        db 0EAh ; far jump ; no return needed as this handler uses retf
        old_mouse_event_offs dw 0  
        old_mouse_event_seg  dw 0

        @@return:
        leave
        retf
        endp

    KeyboardHandler proc
        push ax
        push bx
        mov bx, es
        push bx

        in al, 60h

        cmp al, 52h
        jz @@ins_press

        cmp al, 3bh
        jz @@f1_press
        
        cmp al, 0D2h
        jz @@ins_rel

        @@oldint:
        pop bx
        mov es, bx
        pop bx
        pop ax

        db 0EAh ;long jump
        old_keyboard_int_offs dw 0
        old_keyboard_int_seg  dw 0

        @@f1_press:
        mov bx, ds
        push bx
        mov bx, es
        push bx
        pusha

        call RegisterEventHandlers
        popa
        jmp @@return

        @@ins_press:
            mov ax, ds ;push segments
            push ax
            mov ax, es
            push ax

            mov ax, cs
            mov ds, ax
            setvmema

            mov byte ptr es:[43], 30h
            mov ah, regs_win.window_state
            test ah, 1 ;window busy
            jnz @@return

            pusha
            test ah, 2  ; window hiden
            jnz @@unhide_window

            @@hide_window:
            xor cx, cx ; bc UpdWindowBuffer expects ch=0
            mov bx, offset regs_win
            call UpdWindowBuffer
            mov bx, offset regs_win
            call HideWindow
            popa
            jmp @@return
            @@unhide_window:
            mov bx, offset regs_win
            call UnhideWindow
            popa
            jmp @@return
        @@ins_rel:
            mov ax, ds ;push segments
            push ax
            mov ax, es
            push ax
            setvmema
            mov byte ptr es:[43], 20h

        @@return:
        pop ax
        mov es, ax
        pop ax
        mov ds, ax        

        in  al, 61h
        or  al, 80h
        out 61h, al
        and al, 7Fh
        out 61h, al ;re-enable keyboard

        mov al, 20h ; send EOI
        out 20h, al

        pop bx
        mov es, bx
        pop bx
        pop ax
        iret     
        
    endp

    bp_save dw 0
    TimerHandler proc
        push bx  
        mov bx, es
        push bx
        setvmem
        xor byte ptr es:[1], 0FFh
        pop bx
        mov es, bx
        pop bx

        call UpdRegsWin

        db 0EAh ;long jump
        old_timer_int_offs dw 0
        old_timer_int_seg  dw 0
    endp

    public RegisterEventHandlers
    ; sets mouse event handler
    ; Entry/exit : none
    ; Destroys: ax, bx, cx, dx, es
    RegisterEventHandlers proc
        mov ax, 0001h ;display ptr
        int 33h

        mov bx, cs
        mov es, bx

        mov ax, 0014h ; exchange handlers
        mov cx, 007Fh     ; all events
        mov dx, offset MouseHandler
        int 33h

        ; mouse handler chaining disabled bc Volkov does not need it and does not like it
        ;mov ax, es
        ;mov bx, cs
        ;cmp ax, bx
        ;jz @@return ; if it is our own handler, do nothing to avoid infinite loops
        ;mov old_mouse_event_mask, cx
        ;mov old_mouse_event_offs, dx
        ;mov old_mouse_event_seg , ax
        @@return:
        ret
    endp

    public RemoveEventHandlers
    RemoveEventHandlers proc
        mov bx, cs
        mov es, bx

        mov ax, 000ch ; set event handler
        mov cx, 0     ; disable
        mov dx, 0
        int 33h

        ret
    endp

    public SetInterrupts
    SetInterrupts proc
        mov bx, 0
        mov es, bx

        mov bx , es:[26h] ; keyboard int segment
        mov old_keyboard_int_seg , bx
        mov bx , es:[24h] ; keyboard int offset
        mov old_keyboard_int_offs, bx

        mov bx , es:[22h] ; timer int segment
        mov old_timer_int_seg, bx
        mov bx , es:[20h] ; timer int offset
        mov old_timer_int_offs, bx

        cli
        mov bx, cs
        mov word ptr es:[26h], bx
        mov word ptr es:[24h], offset  KeyboardHandler
        mov word ptr es:[22h], bx
        mov word ptr es:[20h], offset  TimerHandler
        sti

        ret
    endp

    public RemoveInterrupts
    RemoveInterrupts proc
        cli
        mov bx, old_keyboard_int_seg
        mov word ptr es:[26h], bx
        mov bx, old_keyboard_int_offs
        mov word ptr es:[24h], bx
        sti
        ret
    endp


    old_mouse_event_mask dw 0

    db "<....events>"
end