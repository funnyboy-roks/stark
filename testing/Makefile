test: test.o
	gcc -o test -no-pie test.o

test.o: test.s
	fasm test.s test.o

test.s: test.st
	cargo r -- --gen test.st > test.s
