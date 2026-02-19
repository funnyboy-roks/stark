hello: hello.s hello-c.o
	gcc -no-pie -o hello hello.s hello.c

hello.s: hello.st
	cargo r -- hello.st -o hello.s

hello-c.o: hello.c
	gcc -c -o hello-c.o hello.c
