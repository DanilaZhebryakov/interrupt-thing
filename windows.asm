include macros.asm

include screen.inc
include buttons.inc

include winstrct.inc

.model tiny
.code

; Entry: ds:dx = charset addr, ah = color attr ds:bx = window to initialize, cs:di = drag handler
; Exit: none
; Destroys: everything except bx, ds and stack
; Note: window is not displayed by default
public InitWindow
InitWindow proc
    push di ;event handler
    push bx
    push dx

    mov dx, ds
    mov es, dx

    
    mov dx, word ptr ds:[bx][window_width] ;size
    mov si, dx ;row len
    sub dx, 0202h ; frame edges should fit in window
    and si, 00FFh ; mov dl, si
    shl si, 1
    sub si, 4
    mov di, ds:[bx][window_drawbuf] ;draw in buffer
    pop bx ;charset (old dx)
    call OutFrm

    pop si ;old bx

    mov al, ds:[si][window_btngroup]
    call GetFreeBtnAddr
    mov ax, ds:[si][window_pos]
    xor dx, dx
    mov cx, 160
    div cx
    shr dx, 1
    mov dh, al
    mov word ptr ds:[bx][btnx0], dx
    add dx, word ptr ds:[si][window_width]
    mov word ptr ds:[bx][btnx1], dx
    mov word ptr ds:[bx][btntype], 0047h ;draggable, pressable, L-btn (not active by default)
    pop word ptr ds:[bx][btnhandler] ; event handler (old di)
    mov bx, si
    ret
endp


; Entry: ds:bx = window to display
; Exit: none
; Destroys: everything except bx, ds and stack
public UnhideWindow
UnhideWindow proc
    mov ax, ds
    mov es, ax

    or ds:[bx][window_state], 1 ;window busy

    mov cx, btn_count
    mov si, offset screen_buttons
    mov al, ds:[bx][window_btngroup]
    @@btn_loop:
        cmp ds:[si][btngroupid], al
        jnz @@loop_end
        or ds:[si][btntype], 1 ;enable button 
        @@loop_end:
        add si, btn_struc_size
    loop @@btn_loop

    xor ax, ax
    mov dx, word ptr ds:[bx][window_width] ;size
    mov al, dl ;row len
    shl ax, 1
    mov si, ds:[bx][window_pos] ;from screen
    mov di, ds:[bx][window_savebuf] ;to savebuffer

    push bx
    mov bx, 0B800h ;video memory segment
    mov ds, bx

    mov bx, ax
    mov ax, 160 ;screen rowlen


    call WriteBuffer

    mov bx, es
    mov ds, bx
    pop bx

; updates window on screen
; Entry: ds:bx = window to display
; Expects: es = vmem
; Destroys: everything except bx, segments and stack
public UpdWindow
UpdWindow:
    setvmema
    or byte ptr ds:[bx][window_state], 1 ;window busy
    mov dx, word ptr ds:[bx][window_width]
    xor ax, ax
    mov al, dl ;row len
    shl ax, 1
    mov si, ds:[bx][window_drawbuf] ;from drawbuf
    mov di, ds:[bx][window_pos]     ;to screen

    push bx
    mov bx, 160
    call WriteBuffer
    pop bx
    and ds:[bx][window_state], 0FCh ;not busy and not hiden
    ret
endp

; Update save-buffer from screen changes
; Entry: ds:bx = window
; Exit: none
; Expects: es = vmem
; Destroys: everything except stack and segments
public UpdWindowBuffer
UpdWindowBuffer proc
    mov ax, 02h
    int 33h
    or byte ptr ds:[bx][window_state], 1 ;normally you write something just after this, so window busy
    mov si, ds:[bx][window_drawbuf]
    mov di, ds:[bx][window_pos]
    xor ax, ax
    mov dx, word ptr ds:[bx][window_width]
    mov al, dl ;row len
    shl ax, 1
    mov bx, ds:[bx][window_savebuf]
    sub bx, si
    call UpdSaveBuffer
    mov ax, 01h
    int 33h
    ret
endp

; Update save-buffer from screen changes
; Entry: ds:bx = window
; Exit: none
; Expects: es = vmem
; Destroys: everything except stack , segments and bx
public HideWindow
HideWindow proc
    or ds:[bx][window_state], 3 ;window busy and hiden

    mov cx, btn_count
    mov si, offset screen_buttons
    mov al, ds:[bx][window_btngroup]
    @@btn_loop:
        cmp ds:[si][btngroupid], al
        jnz @@loop_end
        and ds:[si][btntype], 0FEh ;disable button 
        @@loop_end:
        add si, btn_struc_size
    loop @@btn_loop

    mov si, ds:[bx][window_savebuf]
    mov di, ds:[bx][window_pos]
    xor ax, ax
    mov dx, word ptr ds:[bx][window_width]
    mov al, dl ;row len
    shl al, 1
    push bx
    mov bx, 160
    call WriteBuffer
    pop bx
    
    and ds:[bx][window_state], 0FEh ; not busy
    
    ret
endp

; Entry: ds:bx = window to move, si = dx, di = dy
; Exit: none
; Destroys: everything except stack , ds , bx
public MoveWindow
MoveWindow proc
    mov ax, ds:[bx][window_pos]
    xor dx, dx
    mov cx, 160
    div cx
    add ax, di
    cmp ax, 25
    ja @@return

    add dx, si
    cmp dx, 160
    ja @@return


    push si
    push di
    setvmema
    push bx
    xor cx, cx
    call UpdWindowBuffer
    pop bx
    call HideWindow
    pop di
    pop si

    mov al, ds:[bx][window_btngroup]
    push bx
    call MvBtnGroup
    pop bx    

    mov ax, di
    mov cx, 80
    imul cl
    add ax, si
    shl ax, 1
    
    add ds:[bx][window_pos], ax
    jmp UnhideWindow
    @@return:
    ret
endp

public BasicDragHandler
; basic drag handler for windows
; moves window at ds:bx by si at x and di at y (char pos) (this is the format this data is given to btn handler)
; Expects btnstate-format flags at al, ds = program ds
; Destroys everything except stack and ds, so pusha/popa needed
; Event handler for each window should be defined, and given to its main button as event handler. No universal handler or window table exist.
; Example handler:
; example_wdh proc ;window drag handler
;        pusha
;        mov al, ds:[bx][btnstate]
;        mov bx, offset example_win
;        call BasicDragHandler
;        popa
;        ret
;    endp
BasicDragHandler proc
    cmp di, 0
    jnz @@normal
    cmp si, 0
    jnz @@normal
    @@return:
    ret
    @@normal:
    test al, 80h ;check if it is drag event
    jz @@return
    test ds:[bx][window_state], 1 ;check if window is not busy
    jnz @@return
    hidemouseptr
    call MoveWindow
    displmouseptr
    ret
endp

end