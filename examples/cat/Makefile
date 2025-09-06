cat: cat.o
	gcc -ggdb -no-pie -o cat cat.o

cat.o: cat.s
	fasm cat.s cat.o

cat.s: cat.st
	cargo r -- --gen cat.st > cat.s
