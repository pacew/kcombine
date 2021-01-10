CFLAGS = -g -Wall

board1.kcad_sch:
	./kcombine.py board1.lisp


clean:
	rm -f board1.kicad_pro board1.kicad_sch board1.kicad_prl
	rm -f board1.kicad_pcb
	rm -f *~

ccombine: ccombine.o
	$(CC) $(CFLAGS) -o ccombine ccombine.o -lm
