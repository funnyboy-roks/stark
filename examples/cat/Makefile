cat: cat.s
	gcc -ggdb -no-pie -o cat cat.s

cat.s: cat.st
	cargo r -- cat.st -o cat.s
