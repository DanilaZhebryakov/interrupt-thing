tasm main.asm
tasm input.asm
tasm output.asm
tasm screen.asm
tasm buttons.asm
tasm windows.asm
tasm events.asm
tasm prog_end.asm


tlink /t main.obj input.obj output.obj screen.obj buttons.obj events.obj windows.obj prog_end.obj
