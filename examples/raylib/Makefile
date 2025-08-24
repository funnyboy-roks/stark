japan: japan.o
	gcc -ggdb -no-pie -o japan japan.o -lraylib

japan.o: japan.s
	fasm japan.s japan.o

japan.s: japan.st
	cargo r -- japan.st -o japan.s
