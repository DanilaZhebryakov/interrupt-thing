button struc
        btntype    db 0 ;bits: 1 = active  2=Lclick  4=pressable 8=Rclick 10h=togglable 20h=Mclick 40h = draggable
        btnstate   db 0 ;bits: 1 =Lclicked 2=pressed 4=Rclicked  8=ON     10h=Mclicked  20h = event_press, 40h = event_release, 80h=dragged
        btngroupid db 0

        btnx0 db 0
        btny0 db 0

        btnx1 db 0
        btny1 db 0

        btnhandler dw 0FFFFh
button ends

btn_struc_size equ 9
btn_count equ 64