hello: hello.o hello-c.o
	gcc -no-pie -o hello hello.o hello.c

hello.o: hello.s
	fasm hello.s hello.o

hello.s: hello.st
	cargo r -- hello.st -o hello.s

hello-c.o: hello.c
	gcc -c -o hello-c.o hello.c
