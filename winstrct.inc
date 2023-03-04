window struc
    window_state    db 2 ;bits: 1 = busy, 2 = hiden
    window_btngroup db 0 ;index of associated button group

    window_pos   dw 0 ;pos (index of char on screen to put into di

    window_width  db 0 ;can also be used directly for loading size as word
    window_height db 0

    window_drawbuf dw 0 ;drawing and saved buffer
    window_savebuf dw 0 
window ends