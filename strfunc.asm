
.model tiny
.code

;
; Calculates string length until first 0 
; Entry: ds:si = string
; Exit: cx = length
; Destroys: al, si
Strlen proc:
    xor al, al
    xor cx, cx
    repne scasb
    neg cx
    inc cx
    ret
endp

;
; Calculates first pos of al in first cx bytes of string at ds:si
; Entry: ds:si = string , cx = len, al = char
; Exit: si = result pointer or 0 if not found
; Destroys: cx
Memchr proc:
    test cx, cx
    jz @@return_0

    repne scasb

    test cx, cx
    jnz @@return
    @@return_0;
    mov si, 1

    @@return:
    dec si
    ret
endp

;
; Calculates first pos of ah in 0-terminated string at ds:si
; Entry: ds:si = string, ah = char
; Exit: si = result pointer or 0 if not found
; Destroys: al
Strchr proc:
    
    @@strchr_loop:
        lodsb
        test al, al
        jz @@return_0

        cmp al, ah
        jz @@return

    jmp @@strchr_loop

    @@return_0:
    mov si, 0
    @@return:
    dec si
    ret
endp

; 
; Sets cx bytes of es:di to ax
; Entry: es:di = string, cx = size
; Exit: none
; Destroys: di, cx

Memset proc
    rep stosb
    ret
endp

; 
; Copy first cx bytes from ds:si to es:di
; Entry: es:di = dst, ds:si = src, cx = len
; Exit: none
; Destroys: si, di , cx

Memcpy proc
    rep movsb
    ret
endp

; 
; Compare first cx bytes of ds:si and es:di
; Entry: es:di = stra, ds:si = strb, cx = len
; Exit: cl = str_a - str_b of first mismatched bytes
; Destroys: si, di, cx

Memcmp proc
    repe cmpsb
    dec si
    dec di
    mov cl, ds:[si]
    sub cl, es:[di]
    ret
endp

; 
; Compare ds:si and es:di (until 0)
; Entry: es:di = stra, ds:si = strb
; Exit: cl = str_a - str_b of first mismatched bytes
; Destroys: si, di, al, cl
Strcmp proc
    @@strcmp_loop:
        lodsb
        mov cl, al
        sub cl, es:[di]
        inc di
        jz @@return

        test al, al
        jz @@return
    jmp @@strcmp_loop
    @@return
    ret
endp



end