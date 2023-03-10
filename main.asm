.model tiny
.code

include macros.asm
include input.inc
include output.inc
include screen.inc
include events.inc
include buttons.inc
include windows.inc
extrn program_end:near

org 100h

regs_win_w equ 10
regs_win_h equ 13

Start:
    jmp main_start	    
    db "<main....>"
    ; bx = btn addr
    ; dl = x, dh = y
    ; si = dx, di = dy
    regs_wdh proc ;window drag handler
        pusha
        mov al, ds:[bx][btnstate]
        mov bx, offset regs_win
        call BasicDragHandler
        popa
        ret
    endp
    
    public UpdRegsWin
    ;called from interrupt handler
    UpdRegsWin proc
        
        pusha
        mov bp, sp
        mov bx, ds
        push bx
        mov bx, es
        push bx
        hidemouseptr

        mov bx, cs
        mov ds, bx

        test regs_win.window_state, 3
        jnz @@return ;busy or hiden
            or regs_win.window_state,1 ;busy

            setvmem
            xor cx, cx
            mov bx, offset regs_win
            call UpdWindowBuffer
            
            mov di, cs
            mov es, di
            mov di, offset regs_win_drawbuf + 2*(regs_win_w + 4) ;skip first line, |reg name:

            mov cx, 10 ;regs count
            mov bx, offset regs_stk_addr
            mov ah, 02h ;color attr
            @@regs_out_loop:
                mov si, ds:[bx]
                add bx, 2
                mov dx, ss:[bp][si]
                mov si, cx
                push bx
                call WriteHex
                pop bx
                add di, 2*(regs_win_w - 4) ;\n\r
                mov cx, si
            loop @@regs_out_loop
            mov dx, ss
            call WriteHex
            mov bx, offset regs_win
            call UpdWindow ;also resets busy flag

        @@return:
        displmouseptr
        pop bx
        mov es, bx
        pop bx
        mov ds, bx
        popa
        
        ret
    endp

    regs_win_drawbuf db (regs_win_w * regs_win_h * 2) dup(0)
    regs_win_savebuf db (regs_win_w * regs_win_h * 2) dup(0)
    public regs_win
    regs_win window <,1,800,regs_win_w, regs_win_h ,offset regs_win_drawbuf, offset regs_win_savebuf> ;window at (5,4) with btngroup 1 and two buffers

    main_start:

        call RegisterEventHandlers
        call SetInterrupts

        mov bx, offset regs_win
        mov dx, offset charset2
        mov ah, 02h
        mov di, offset regs_wdh
        call InitWindow

        mov di, ds
        mov es, di
        mov di, offset regs_win_drawbuf + 2*(regs_win_w+1)
        mov si, offset regs_str
        mov cx, 11 ;regs count
        @@rstr_loop:
            movsb
            inc di
            movsb
            inc di
            movsb
            inc di
            add di, 2*(regs_win_w-3) ;\n\r
        loop @@rstr_loop

        call UnhideWindow
        push bp
        mov ax, 1111h
        mov bx, 2222h
        mov cx, 3333h
        mov dx, 4444h
        mov si, 5555h
        mov di, 6666h
        mov bp,0BBBBh
        call UpdRegsWin
        pop bp

        mov cx, 100
        wloop:
            mov dx, 0
            wloop_int:
            dec dx
            jnz wloop_int
        loop wloop

        ;call RemoveEventHandlers
        ;call RemoveInterrupts

        mov dx, offset program_end + 1
        int 27h


    charset0 db 9 dup(0)
    charset1 db 0dah, 0c4h, 0bfh, 0b3h, ' ' , 0b3h, 0c0h, 0c4h, 0d9h
    charset2 db 0c9h, 0cdh, 0bbh, 0bah, ' ' , 0bah, 0c8h, 0cdh, 0bch
    charset3 db 0d6h, 0c4h, 0b7h, 0bah, ' ' , 0bah, 0d3h, 0c4h, 0bdh
    charset4 db 0d5h, 0cdh, 0b8h, 0b3h, ' ' , 0b3h, 0d4h, 0cdh, 0beh

    regs_str db "ax:bx:cx:dx:si:di:sp:bp:ds:es:ss:"
    regs_stk_addr dw 14,8,12,10,2,0,6,4,-2,-4
    
    public free_mem
    free_mem db 1024 dup(0)
    db "<....main>"
end Start